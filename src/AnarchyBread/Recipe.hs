module AnarchyBread.Recipe (
  allRecipes,
  pprAllRecipes,
) where

import AnarchyBread.Recipe.Raw
import AnarchyBread.Types
import Control.Monad
import qualified Data.Text as T
import qualified Data.Vector as V
import AnarchyBread.Emoji
import Data.List
import Data.Foldable


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

pprAllRecipes :: IO ()
pprAllRecipes = do
  let itemStr v = ":" <> T.unpack (itemToEmoji v) <> ":"
  putStrLn "Known recipes are as follows:"
  forM_ (zip (universe @Item) $ V.toList allRecipes) \(i, rs) ->
    unless (null rs) do
      putStrLn $ "Target: " <> itemStr i <> ", " <> show (length rs) <> " recipes."
      forM_ (zip [1 :: Int ..] rs) \(ir, comps) -> do
        let ppr (itm, cnt) = itemStr itm <> " x" <> show cnt
        putStrLn $ "- " <> show ir <> ": " <> intercalate ", " (toList $ fmap ppr comps)

      putStrLn ""
