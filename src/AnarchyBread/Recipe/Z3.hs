module AnarchyBread.Recipe.Z3 (
  RecipeRef,
  Solution,
  maximizeItem,
) where

import AnarchyBread.Emoji
import AnarchyBread.Recipe.Filter
import AnarchyBread.Types
import Control.Monad
import Control.Monad.Writer.CPS
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Z3.Monad
import Shower

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

{-
  Unchecked assumption: goal item must be involved in at least one side
  of a recipe.
 -}
maximizeItem :: Item -> RecipeSet -> (Item -> Int) -> IO (Maybe Solution)
maximizeItem goal rSet getItem =
  snd <$> do
    let logic = Just QF_NIA
        refs :: [RecipeRef]
        refs = do
          (i, inds) <- zip [0 ..] (toList rSet)
          let item = toEnum @Item i
          j <- inds
          pure (item, j)

    let costsAndGains = computeCostGains rSet
        itemsInvolved = M.keysSet costsAndGains

    printer costsAndGains
    evalZ3With logic stdOpts do
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
      let initCount = fromIntegral $ getItem goal
          {-
            For all current recipes, exactly one item is produced, costing at least one item.
            therefore it should be a safe assumption that whatever we can produce, it has to be
            less than this amount.

            Safety of using maximum is based on input assumption: it is only safe
            when `itemsInvolved` is not empty.
           -}
          initHi = 1 + initCount + fromIntegral (maximum (fmap getItem (S.toList itemsInvolved)))
          initRange :: (Integer, Integer)
          initRange = (initCount, initHi)
      {-
        Z3 maximization is not very reliable for this model,
        instead we do our own binary search - range = (lo, hi) has the invariant that:

        - lo < hi
        - lo is always satisfiable
        - hi is never satisfiable

        we can maximize production by testing whether an exact amount is satisfiable.

       -}
      ans <-
        fix
          ( \go (lo, hi) -> do
              let mid = quot (lo + hi) 2
              if mid <= lo
                then pure lo
                else do
                  (_sat, r) <- local do
                    assert =<< mkEq goalVar =<< mkInteger mid
                    withModel \m -> do
                      Just v <- evalInt m goalVar
                      pure v
                  case r of
                    Just _ -> go (mid, hi)
                    Nothing -> go (lo, mid)
          )
          initRange
      assert =<< mkEq goalVar =<< mkInteger ans
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
