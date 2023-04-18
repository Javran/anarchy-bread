module AnarchyBread.Recipe.Filter (
  allRecipes,
  RecipeFilter,
  apply,
  RecipeSet,
  isNormalGemRecipe,
  normalGemRecipes,
  isNormalChessRecipe,
  normalChessRecipes,
  getRecipe,
) where

import AnarchyBread.Recipe.Raw
import AnarchyBread.Recipe.Types
import AnarchyBread.Types
import qualified Data.List.NonEmpty as NE
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

getRecipe :: Item -> Int -> Maybe Recipe
getRecipe item ind = (V.!? ind) $ V.unsafeIndex allRecipes (fromEnum item)

isGemChessatronRecipe :: RecipeFilter
isGemChessatronRecipe dst srcs = (dst, srcs) == (Chessatron, NE.singleton (Gem GRed, 16))

isNormalGemRecipe :: RecipeFilter
isNormalGemRecipe dst srcs = case dst of
  Gem cDst ->
    all
      ( \case
          (Gem cSrc, _) -> cDst > cSrc
          _ -> False
      )
      srcs
  _ -> False

normalGemRecipes :: RecipeSet
normalGemRecipes = apply isNormalGemRecipe

isNormalChessRecipe :: RecipeFilter
isNormalChessRecipe dst srcs = case dst of
  ChessPiece {} ->
    any
      ( \case
          (Gem _, _) -> False
          _ -> True
      )
      srcs
  Chessatron ->
    not (isGemChessatronRecipe dst srcs)
  _ ->
    all
      ( \case
          (Bread Loaf, _) -> True
          _ -> False
      )
      srcs

normalChessRecipes :: RecipeSet
normalChessRecipes = apply isNormalChessRecipe
