module AnarchyBread.Gold (
  subCmd,
) where

import AnarchyBread.Account as Account
import AnarchyBread.Parse
import AnarchyBread.Recipe.Filter
import AnarchyBread.Recipe.Z3
import AnarchyBread.Types
import qualified Data.Map.Strict as M
import Data.Maybe
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

subCmd :: SubCmd
subCmd cmdPrefix =
  getArgs >>= \case
    ["account"] -> do
      GAccount {inventory} <- Account.loadFromEnv
      experiment normalGemRecipes (getByItem inventory)
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
          experiment normalGemRecipes getItem
        r -> die $ "parse error, left: " <> show r
    args -> do
      putStrLn $ "Unknown: " <> show args
      putStrLn $ cmdPrefix <> " account: compute from account config"
      {-
        e.g. <prog> parse 570 :gem_red: , 453 :gem_blue: , 127 :gem_purple: , 26 :gem_green: , 22 :gem_gold:
       -}
      putStrLn $ cmdPrefix <> " parse <message>: compute from Discord message"
      pure ()
