module AnarchyBread.Recipe.Z3 (
  experiment,
) where

import AnarchyBread.Account as Account
import AnarchyBread.Emoji
import AnarchyBread.Recipe.Filter
import AnarchyBread.Types
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Writer.CPS
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import Z3.Monad

-- Reference to a recipe
type RecipeRef = (Item, Int)

getByItem :: VU.Unbox a => VU.Vector a -> Item -> a
getByItem v i = VU.unsafeIndex v (fromEnum i)

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

experiment :: RecipeSet -> IO ()
experiment rSet = do
  let logic = Just QF_NIA
      refs :: [RecipeRef]
      refs = do
        (i, inds) <- zip [0 ..] (toList rSet)
        let item = toEnum @Item i
        j <- inds
        pure (item, j)

  let costsAndGains = computeCostGains rSet
      itemsInvolved = M.keysSet costsAndGains

  GAccount {inventory} <- Account.loadFromEnv
  result <- evalZ3With logic stdOpts do
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
    let goldVar = itemOutVars M.! Gem GGold
    forM_ (M.toList costsAndGains) $ \(item, (costs, gains)) -> do
      let itemIn :: Integer
          itemIn = fromIntegral (getByItem inventory item)
          outVar = itemOutVars M.! item
      orig <- mkInteger itemIn
      (totCost :: AST) <-
        mkUnaryMinus =<< do
          xs :: [AST] <- forM costs \(ref, cnt) -> do
            let rVar = recipeUseVars M.! ref
            c <- mkInteger (fromIntegral cnt)
            mkMul [rVar, c]
          mkAdd xs
      (totGain :: AST) <- do
        xs :: [AST] <- forM gains \(ref, cnt) -> do
          let rVar = recipeUseVars M.! ref
          c <- mkInteger (fromIntegral cnt)
          mkMul [rVar, c]
        mkAdd xs
      net <- mkAdd [orig, totCost, totGain]
      assert =<< mkEq outVar net
    let initCount :: Integer
        initCount = fromIntegral $ getByItem inventory (Gem GGold)
        initHi = initCount + fromIntegral (maximum (fmap (\i -> getByItem inventory i) (S.toList itemsInvolved)))
        initRange :: (Integer, Integer)
        initRange = (initCount, initHi)
    ans <-
      fix
        ( \go (lo, hi) -> do
            let mid = quot (lo + hi) 2
            if mid <= lo
              then pure lo
              else do
                (_sat, r) <- local do
                  assert =<< mkEq goldVar =<< mkInteger mid
                  withModel \m -> do
                    Just v <- evalInt m goldVar
                    pure v
                case r of
                  Just _ -> go (mid, hi)
                  Nothing -> go (lo, mid)
        )
        initRange
    s <- mkInteger ans
    assert =<< mkEq s goldVar
    withModel \m -> do
      liftIO $ putStrLn "Inventory changes:"
      forM_ (zip (S.toAscList itemsInvolved) $ M.toAscList itemOutVars) \(item, (_, var)) -> do
        let itemIn = getByItem inventory item
        ~(Just v) <- evalInt m var
        liftIO $ putStrLn $ ":" <> T.unpack (itemToEmoji item) <> ": " <> show itemIn <> " -> " <> show v

      liftIO $ do
        putStrLn ""
        putStrLn "Recipe use:"
      forM_ refs \ref@(item, i) -> do
        let var = recipeUseVars M.! ref
        ~(Just v) <- evalInt m var
        liftIO $ putStrLn $ T.unpack (itemToEmoji item) <> "/" <> show (i + 1) <> " x" <> show v
      pure ()
  print result
