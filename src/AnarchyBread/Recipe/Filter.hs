module AnarchyBread.Recipe.Filter (
  allRecipes,
  RecipeFilter,
  apply,
  RecipeSet,
  isNormalGemRecipe,
  normalGemRecipes,
) where

import AnarchyBread.Recipe.Raw
import AnarchyBread.Recipe.Types
import AnarchyBread.Types
import qualified Data.Vector as V

type RecipeFilter = Item -> Recipe -> Bool

{-
  Index references to `allRecipes`
 -}
type RecipeSet = V.Vector [Int]

apply :: RecipeFilter -> RecipeSet
apply rf = V.imap go allRecipes
  where
    go i rs =
      foldMap
        (\(j, rp) -> [j | rf (toEnum i) rp])
        (zip [0 ..] (V.toList rs))

isNormalGemRecipe :: RecipeFilter
isNormalGemRecipe dst srcs = case dst of
  Gem cDst ->
    all
      ( \(src, _) -> case src of
          Gem cSrc -> cDst > cSrc
          _ -> False
      )
      srcs
  _ -> False

normalGemRecipes :: RecipeSet
normalGemRecipes = apply isNormalGemRecipe
