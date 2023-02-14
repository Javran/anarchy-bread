module AnarchyBread.Gold (
  subCmd,
) where

import AnarchyBread.Account as Account
import AnarchyBread.Recipe.Filter
import AnarchyBread.Recipe.Z3
import AnarchyBread.Types
import qualified Data.Vector.Unboxed as VU
import System.Environment

getByItem :: VU.Unbox a => VU.Vector a -> Item -> a
getByItem v i = VU.unsafeIndex v (fromEnum i)

subCmd :: SubCmd
subCmd cmdPrefix =
  getArgs >>= \case
    ["account"] -> do
      GAccount {inventory} <- Account.loadFromEnv
      experiment normalGemRecipes (getByItem inventory)
    "parse" : args -> do
      print args
    args -> do
      putStrLn $ "Unknown: " <> show args
      putStrLn $ cmdPrefix <> " account: compute from account config"
      {-
        TODO: not implemented yet
        e.g. <prog> parse 570 :gem_red: , 453 :gem_blue: , 127 :gem_purple: , 26 :gem_green: , 22 :gem_gold:
       -}
      putStrLn $ cmdPrefix <> " parse <message>: compute from Discord message"
      pure ()
