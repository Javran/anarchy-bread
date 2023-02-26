module AnarchyBread.Recipe.Z3 (
  RecipeRef,
  Solution,
  maximizeItem,
  maximizeItemLim,
) where

import AnarchyBread.Emoji
import AnarchyBread.Recipe.Filter
import AnarchyBread.Types
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Cont
import Control.Monad.Writer.CPS
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import System.IO
import Z3.Monad

-- Reference to a recipe
type RecipeRef = (Item, Int)

type CostGain =
  ( {- cost -} [(RecipeRef, Int)]
  , {- gain -} [(RecipeRef, Int)]
  )

{-
  Rearranges recipe set in terms of items - for each item involved,
  collect recipe, and how many of that item is invoved,
  and whether item is on "cost" side or "gain" side of the recipe.
 -}
computeCostGains :: RecipeSet -> M.Map Item CostGain
computeCostGains rSet = M.fromListWith (<>) $ execWriter do
  let xs = do
        (i, inds) <- zip [0 ..] (toList rSet)
        let item = toEnum @Item i
        j <- inds
        Just srcs <- pure $ getRecipe item j
        let ref = (item, j)
        pure (ref, item, srcs)
  forM_ xs \(ref, dst, srcs) -> do
    tell [(dst, (mempty, [(ref, 1)]))]
    forM_ srcs \(src, cnt) ->
      tell [(src, ([(ref, fromIntegral cnt)], mempty))]

type Solution = (M.Map Item (Integer, Integer), M.Map RecipeRef Integer)

penalty :: Item -> Integer
penalty = \case
  Bread b -> case b of
    Loaf -> 1
    _ | elem b specialBreads -> 10
    _ | elem b rareBreads -> 25
    _ -> error $ "no category for " <> show b
  ChessPiece c p ->
    let cFac = case c of
          Black -> 1
          White -> 3
        pFac = case p of
          Pawn -> 100
          Knight -> 400
          Bishop -> 400
          Rook -> 400
          Queen -> 800
          King -> 800
     in cFac * pFac
  Gem c ->
    let n = 16384
     in case c of
          GRed -> n
          GBlue -> n * 2
          GPurple -> n * 4
          GGreen -> n * 8
          GGold -> n * 8 * 4
  v -> error $ "should not appear on cost side of a recipe: " <> show v

maximizeItem :: Item -> RecipeSet -> (Item -> Int) -> IO (Either String Solution)
maximizeItem t r f = maximizeItemLim t r f Nothing

maximizeItemLim :: Item -> RecipeSet -> (Item -> Int) -> Maybe Int -> IO (Either String Solution)
maximizeItemLim goal rSet getItem lim = runExceptT do
  let logic = Just QF_NIA
      costsAndGains = computeCostGains rSet
      goalCheck = do
        {-
          Checks that selected set of recipes is at least capable of
          producing a positive amount of goal item.

          Passing this check also means that we have a non-empty set of recipes.
         -}
        (_, gs) <- costsAndGains M.!? goal
        guard $ foldMap (Sum . snd) gs > 0

  when (isNothing goalCheck) do
    throwError "Goal item not produced by selected set of recipes."

  forM_ lim \v -> when (v <= 0) do
    throwError "Limit should be positive."

  (_, r) <- lift $ evalZ3With logic stdOpts $ runModel goal rSet costsAndGains getItem lim
  case r of
    Nothing -> throwError "Unsatisfiable."
    Just v -> pure v

{-
  Note that this model works under few assumptions:

  - `costsAndGains` is non-empty.
  - `lim`, when given, is a positive number.

 -}
runModel :: Item -> RecipeSet -> M.Map Item CostGain -> (Item -> Int) -> Maybe Int -> Z3 (Result, Maybe Solution)
runModel goal rSet costsAndGains getItem mUserLim = do
  let refs :: [RecipeRef]
      refs = do
        (i, inds) <- zip [0 ..] (toList rSet)
        let item = toEnum @Item i
        j <- inds
        pure (item, j)

      itemsInvolved = M.keysSet costsAndGains
  z <- mkInteger 0
  -- build up recipe use variables
  recipeUseVars <-
    M.fromList <$> forM refs \ref@(item, i) -> do
      v <- mkFreshIntVar (T.unpack (itemToEmoji item) <> "/" <> show i)
      assert =<< mkGe v z
      pure (ref, v)
  -- item variables
  itemOutVars <-
    M.fromList <$> forM (S.toList itemsInvolved) \item -> do
      v <- mkFreshIntVar (T.unpack (itemToEmoji item) <> "/out")
      assert =<< mkGe v z
      pure (item, v)
  vPenalty <- do
    v <- mkFreshIntVar "penalty"
    assert =<< mkGe v z
    pure v
  let goalVar = itemOutVars M.! goal
  forM_ (M.toList costsAndGains) $ \(item, (costs, gains)) -> do
    let itemIn = fromIntegral (getItem item)
        outVar = itemOutVars M.! item
    orig <- mkInteger itemIn
    totCost <-
      mkUnaryMinus
        =<< mkAdd
        =<< forM costs \(ref, cnt) -> do
          let rVar = recipeUseVars M.! ref
          c <- mkInteger (fromIntegral cnt)
          mkMul [rVar, c]

    totGain <- do
      mkAdd =<< forM gains \(ref, cnt) -> do
        let rVar = recipeUseVars M.! ref
        c <- mkInteger (fromIntegral cnt)
        mkMul [rVar, c]

    net <- mkAdd [orig, totCost, totGain]
    assert =<< mkEq outVar net
  -- build penalty term.
  do
    xs <-
      mkAdd =<< do
        forM (M.toList costsAndGains) $ \(item, (costs, _)) -> do
          ys <- forM costs \(ref, cnt) ->
            if cnt == 0
              then pure z
              else do
                pFac <- mkInteger (penalty item)
                let rVar = recipeUseVars M.! ref
                cnt' <- mkInteger $ fromIntegral cnt
                mkMul [rVar, pFac, cnt']
          mkAdd ys
    assert =<< mkEq vPenalty xs

  {-
    Z3 maximization is not very reliable for this model,
    instead we do our own binary search - range = (lo, hi) has the invariant that:

    - lo < hi
    - lo is always satisfiable
    - hi is never satisfiable

    we can maximize production by testing whether an exact amount is satisfiable.

    In addition we choose to search on # of goal items possible rather than
    recipe uses so that when there are multiple recipe that can directly produce
    the goal items, we don't have to keep track all of them.

   -}
  ans <- evalContT $ callCC \done -> do
    let goalIn = fromIntegral $ getItem goal
        checkGoalEq :: Integer -> Z3 (Maybe Integer)
        checkGoalEq vGoal = do
          (_sat, r) <- local do
            assert =<< mkEq goalVar =<< mkInteger vGoal
            withModel \m -> do
              Just v <- evalInt m goalVar
              pure v
          pure r

    {-
      We want an unsatisfiable upper bound to initiate the search.

      Current observation is that, for all current recipes,
      exactly one item is produced, costing at least one item.
      Therefore it should be a safe assumption that whatever we can produce,
      it has to be less than this amount.

      Safety of using maximum is based on input assumption: it is only safe
      when `itemsInvolved` is not empty.

      Also +1 on top of all this - this is probably unnecessary but it's easier to argue
      that we have an unsatisfiable upperbound this way.
     -}
    let unsatGoal = 1 + goalIn + fromIntegral (maximum (fmap getItem (S.toList itemsInvolved)))

    mLim :: Maybe Integer <- forM mUserLim \vPre -> do
      -- note that vPre is the extra so we need to add original count (goalIn) to it.
      let v = goalIn + fromIntegral vPre
      -- don't trust user input, make sure it's capped at our calculated upperbound.
      if v > unsatGoal
        then do
          lift $ liftIO $ hPutStrLn stderr $ "Adjusting upperbound from " <> show v <> " to " <> show unsatGoal <> "."
          pure unsatGoal
        else do
          pure v

    let initHi = case mLim of
          Nothing -> unsatGoal
          Just lim -> min unsatGoal lim

    {-
      A final check before entering binary search. We want to make sure the upperbound
      is indeed the unsatisfiable upperbound for binary search.

      This might not be the case because:

      - User might give an upperbound that is satisfiable.
      - We might have miscalculated unsatGoal somehow (I don't it's likely though).

      In either case, if initHi is satisfiable, we can forgo the whole search process
      because upperbound of the search space is reachable.
     -}
    lift (checkGoalEq initHi) >>= \case
      Nothing -> pure ()
      Just v -> do
        lift $ liftIO $ hPutStrLn stderr $ "No need for search, limit " <> show v <> " is satisfiable."
        done v

    let bSearch :: Z3 Integer
        bSearch =
          fix
            ( \go (lo, hi) -> do
                let mid = quot (lo + hi) 2
                if mid <= lo
                  then pure lo
                  else
                    checkGoalEq mid >>= \case
                      Just _ -> go (mid, hi)
                      Nothing -> go (lo, mid)
            )
            (goalIn, initHi)

    lift bSearch

  -- fix model to the best known result.
  assert =<< mkEq goalVar =<< mkInteger ans

  {-
    optimize recipe use, targeting minimal penalty.
    note that we no longer have the monotonic property so
    binary search can't be used, instead we just get current
    penalty and see if we can force its value down repetitively.

    TODO: evaluate if we need to limit # of iterations before aborting.
    Z3 seems to be fragile when those repetitive loops are involved.
   -}
  ~(Sat, Just initHiP) <- withModel \m -> do
    ~(Just p) <- evalInt m vPenalty
    pure p
  (ansP, iterCount :: Int) <-
    fix
      ( \go cur cnt -> do
          (_sat, r) <- local do
            assert =<< mkLt vPenalty =<< mkInteger cur
            withModel \m -> do
              Just v <- evalInt m vPenalty
              pure v
          case r of
            Just v -> go v (cnt + 1)
            Nothing -> pure (cur, cnt)
      )
      initHiP
      0
  liftIO $ do
    hPutStrLn stderr $ "Optimized after " <> show iterCount <> " iterations."
    hPutStrLn stderr $ "Penalty improvement (before, after): " <> show (initHiP, ansP)
  assert =<< mkEq vPenalty =<< mkInteger ansP
  withModel \m -> do
    itemChanges :: (M.Map Item (Integer, Integer)) <-
      M.fromList
        <$> forM (M.toList itemOutVars) \(item, vOut) -> do
          ~(Just cntOut) <- evalInt m vOut
          pure (item, (fromIntegral (getItem item), cntOut))
    recipeUses :: (M.Map RecipeRef Integer) <-
      M.fromList
        <$> forM (M.toList recipeUseVars) \(ref, var) -> do
          ~(Just v) <- evalInt m var
          pure (ref, v)
    pure (itemChanges, recipeUses)
