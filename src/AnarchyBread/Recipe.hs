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
