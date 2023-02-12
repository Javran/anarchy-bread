{-# LANGUAGE QuasiQuotes #-}

module AnarchyBread.Recipe.Raw (
  Recipe,
  AllRecipes,
  allRecipes,
) where

import AnarchyBread.Emoji
import AnarchyBread.Types
import Control.Monad
import Data.Char
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.ParserCombinators.ReadP
import Text.RawString.QQ

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
type AllRecipes = V.Vector [Recipe]

intP :: (Integral i, Read i) => ReadP i
intP = read <$> munch1 isDigit

itemP :: ReadP Item
itemP = between (char ':') (char ':') do
  {-
    Ref: https://support.discord.com/hc/en-us/articles/360036479811-Custom-Emojis

    > Emoji names must be at least 2 characters long and can only contain alphanumeric characters and underscores
   -}
  raw <- munch1 (\ch -> ch == '_' || isDigit ch || isAsciiLower ch || isAsciiUpper ch)
  Right item <- pure $ emojiToEItem (T.pack raw)
  _ <- void (char '~' *> intP @Int) <++ pure ()
  pure item

recipeLineP :: ReadP (Int, Recipe)
recipeLineP = do
  n <- between (char '[' <* skipSpaces) (char ']' <* skipSpaces) (intP <* skipSpaces)
  xs <- sepBy1 ((,) <$> (intP @Word <* char ' ') <*> itemP) (string "," <* skipSpaces)
  let m :: M.Map Item Word
      m = M.fromListWithKey (\k -> error $ "Duplicated source item on key " <> show k) $ foldMap go xs
        where
          go (cnt, item) =
            if cnt <= 0
              then error $ "Item count should be positive, got " <> show cnt
              else [(item, cnt)]
  pure (n, NE.fromList (M.toAscList m))

recipeChunkP :: ReadP (Item, [Recipe])
recipeChunkP = do
  n <- between (string "There are ") (string " recipes for ") intP
  target <- itemP <* string ".\n"
  (rs0 :: [] (Int, Recipe)) <- replicateM n (recipeLineP <* char '\n')
  let (ns, rs1) = unzip rs0
      rs2 =
        if ns == [1 .. n]
          then rs1
          else error $ "Recipe index unexpected: " <> show (n, ns)
  pure (target, rs2)

allRecipes :: AllRecipes
allRecipes = case readP_to_S (skipSpaces *> sepBy1 recipeChunkP (char '\n') <* skipSpaces <* eof) rawAllRecipes of
  [(vs :: [] (Item, [Recipe]), "")] -> V.generate (1 + fromEnum (maxBound @Item)) (fromMaybe [] . (\k -> lookup k vs) . toEnum @Item)
  _ -> error "parse error on rawAllRecipes"

{-# INLINE rawAllRecipes #-}
rawAllRecipes :: String
rawAllRecipes =
  [r|
There are 3 recipes for :Wpawn:.
[ 1 ]    2 :Bpawn:,  10 :doughnut:,  10 :bagel:,  10 :waffle:
[ 2 ]    2 :Bpawn:,  10 :croissant:,  10 :flatbread:,  10 :stuffed_flatbread:,  10 :sandwich:,  10 :french_bread:
[ 3 ]    3 :Bpawn:

There are 4 recipes for :Wknight:.
[ 1 ]    1 :Bknight:,  50 :croissant:,  25 :bagel:
[ 2 ]    2 :Bknight:,  50 :croissant:
[ 3 ]    3 :Bknight:
[ 4 ]    2 :Bknight:,  75 :bagel:

There are 4 recipes for :Wbishop:.
[ 1 ]    1 :Bbishop:,  50 :french_bread:,  25 :doughnut:
[ 2 ]    2 :Bbishop:,  50 :french_bread:
[ 3 ]    3 :Bbishop:
[ 4 ]    2 :Bbishop:,  75 :doughnut:

There are 4 recipes for :Wrook:.
[ 1 ]    1 :Brook:,  50 :sandwich:,  25 :waffle:
[ 2 ]    2 :Brook:,  50 :sandwich:
[ 3 ]    3 :Brook:
[ 4 ]    2 :Brook:,  75 :waffle:

There are 4 recipes for :Wqueen:.
[ 1 ]    1 :Bqueen:,  50 :stuffed_flatbread:,  25 :doughnut:
[ 2 ]    2 :Bqueen:,  50 :stuffed_flatbread:
[ 3 ]    3 :Bqueen:
[ 4 ]    2 :Bqueen:,  75 :doughnut:

There are 4 recipes for :Wking:.
[ 1 ]    1 :Bking:,  50 :flatbread:,  25 :bagel:
[ 2 ]    2 :Bking:,  50 :flatbread:
[ 3 ]    3 :Bking:
[ 4 ]    2 :Bking:,  75 :bagel:

There are 1 recipes for :gem_red:.
[ 1 ]    1 :gem_blue:

There are 2 recipes for :gem_blue:.
[ 1 ]    2 :gem_red:
[ 2 ]    1 :gem_purple:

There are 2 recipes for :gem_purple:.
[ 1 ]    2 :gem_blue:
[ 2 ]    1 :gem_green:

There are 1 recipes for :gem_green:.
[ 1 ]    2 :gem_purple:

There are 2 recipes for :gem_gold:.
[ 1 ]    2 :gem_green:,  4 :gem_purple:,  8 :gem_blue:,  16 :gem_red:
[ 2 ]    10000 :bread:,  1000 :croissant:,  1000 :flatbread:,  1000 :stuffed_flatbread:,  1000 :sandwich:,  1000 :french_bread:,  500 :doughnut:,  500 :bagel:,  500 :waffle:

There are 1 recipes for :croissant:.
[ 1 ]    10 :bread:

There are 1 recipes for :flatbread:.
[ 1 ]    10 :bread:

There are 1 recipes for :stuffed_flatbread:.
[ 1 ]    10 :bread:

There are 1 recipes for :sandwich:.
[ 1 ]    10 :bread:

There are 1 recipes for :french_bread:.
[ 1 ]    10 :bread:

There are 1 recipes for :doughnut:.
[ 1 ]    25 :bread:

There are 1 recipes for :bagel:.
[ 1 ]    25 :bread:

There are 1 recipes for :waffle:.
[ 1 ]    25 :bread:

There are 1 recipes for :omega_chessatron:.
[ 1 ]    5 :chessatron:,  1 :anarchy_chess:,  1 :gem_gold:,  1 :gem_green:,  1 :gem_purple:,  1 :gem_blue:,  1 :gem_red:

There are 1 recipes for :anarchy:.
[ 1 ]    5 :anarchy_chess:

There are 1 recipes for :holy_hell~1:.
[ 1 ]    5 :anarchy_chess:

There are 1 recipes for :horsey:.
[ 1 ]    5 :anarchy_chess:
|]
