module AnarchyBread.Gold (
  subCmd,
) where

import AnarchyBread.Account as Account
import AnarchyBread.Emoji
import AnarchyBread.Parse
import AnarchyBread.Recipe.Filter
import AnarchyBread.Recipe.Z3
import AnarchyBread.Types
import Control.Monad
import Data.Either.Extra (eitherToMaybe)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import System.Environment
import System.Exit
import Text.ParserCombinators.ReadP

getByItem :: VU.Unbox a => VU.Vector a -> Item -> a
getByItem v i = VU.unsafeIndex v (fromEnum i)

discordGemLineP :: ReadP [(Item, Int)]
discordGemLineP = many1 do
  cnt <- intP <* skipSpaces
  item <- itemP <* skipSpaces
  pure (item, cnt)

pprResult :: M.Map Item (Integer, Integer) -> M.Map RecipeRef Integer -> IO ()
pprResult invChanges recipeUses =
  do
    putStrLn "Inventory changes:"
    forM_ (M.toAscList invChanges) \(item, (cntIn, cntOut)) -> do
      putStrLn $ ":" <> T.unpack (itemToEmoji item) <> ": " <> show cntIn <> " -> " <> show cntOut
    putStrLn ""

    putStrLn "Recipe use:"
    forM_ (M.toAscList recipeUses) \((item, i), cnt) -> do
      putStrLn $ T.unpack (itemToEmoji item) <> "/" <> show (i + 1) <> " x" <> show cnt

solveForGold :: (Item -> Int) -> IO (Maybe Solution)
solveForGold getItem = eitherToMaybe <$> maximizeItem (Gem GGold) normalGemRecipes getItem

subCmd :: SubCmd
subCmd cmdPrefix =
  getArgs >>= \case
    ["account"] -> do
      GAccount {inventory} <- Account.loadFromEnv
      r <- solveForGold (getByItem inventory)
      case r of
        Nothing -> die "Solver failed"
        Just (x, y) -> pprResult x y
    "parse" : args -> do
      let raw = unwords $ filter (/= ",") args
      case readP_to_S
        ( between
            skipSpaces
            skipSpaces
            discordGemLineP
            <* eof
        )
        raw of
        [(vs, "")] -> do
          let m = M.fromList vs
              getItem = fromMaybe 0 . (m M.!?)
          r <- solveForGold getItem
          case r of
            Nothing -> die "Solver failed"
            Just (x, y) -> pprResult x y
        r -> die $ "parse error, left: " <> show r
    args -> do
      putStrLn $ "Unknown: " <> show args
      putStrLn $ cmdPrefix <> " account: compute from account config"
      {-
        e.g. <prog> parse 570 :gem_red: , 453 :gem_blue: , 127 :gem_purple: , 26 :gem_green: , 22 :gem_gold:
       -}
      putStrLn $ cmdPrefix <> " parse <message>: compute from Discord message"
      pure ()
