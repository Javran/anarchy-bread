module AnarchyBread.Recipe (
  allRecipes,
  pprAllRecipes,
) where

import AnarchyBread.Emoji
import AnarchyBread.Recipe.Filter
import Control.Monad
import Data.Foldable
import Data.List
import qualified Data.Text as T
import qualified Data.Vector as V

{-
  TODO: find a way to craft chessatrons.

  Z3 is possible but it might be too slow and fragile to handle the request.

  We can instead try to impl a simple solver:

  - For simplicity, it will only use recipe that consumes specials / rares / chess pieces,
    but never those that uses gem or convert normal bread into special or rare.

  - Assume a function that try to craft exactly X chessatrons,
    on top of that we can binary search to find the maximum.

  - For a specific target, remaining recipe are attempted in that order,
    in a greedy manner - meet as much demand as possible with first recipe,
    then move on to try next, and so on.

 -}

pprRecipes :: RecipeSet -> IO ()
pprRecipes rSet = do
  let itemStr v = ":" <> T.unpack (itemToEmoji v) <> ":"
  putStrLn "Selected recipes are as follows:"
  forM_ (zip [0 ..] $ V.toList allRecipes) \(i, rs) -> do
    let item = toEnum i
        selectedIndices = rSet V.! i
        selected = fmap (\j -> rs V.! j) selectedIndices
    unless (null rs || null selected) do
      putStrLn $ "Target: " <> itemStr item <> ", " <> show (length selected) <> " recipes."
      forM_ (zip selectedIndices selected) \(ir, comps) -> do
        let ppr (itm, cnt) = itemStr itm <> " x" <> show cnt
        putStrLn $ "/" <> show (ir + 1) <> ": " <> intercalate ", " (toList $ fmap ppr comps)

      putStrLn ""

pprAllRecipes :: IO ()
pprAllRecipes = do
  pprRecipes normalGemRecipes
