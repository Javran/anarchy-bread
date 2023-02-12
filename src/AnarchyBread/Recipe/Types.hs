module AnarchyBread.Recipe.Types (
  Recipe,
  AllRecipes,
) where

import AnarchyBread.Types
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V

{-
  A Recipe should have the following (unchecked) properties:

  - sorted by Item.
  - no duplicated Item.
  - Word > 0

 -}
type Recipe = NE.NonEmpty (Item, Word)

{-
  All recipes in the game, indexed by fromEnum of target Item.
 -}
type AllRecipes = V.Vector (V.Vector Recipe)
