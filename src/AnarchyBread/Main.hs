module AnarchyBread.Main (
  main,
) where

import AnarchyBread.Account as Account
import AnarchyBread.Recipe.Filter
import AnarchyBread.Recipe.Z3
import qualified AnarchyBread.Roll as Roll
import AnarchyBread.Types
import Control.Monad
import qualified Data.Vector.Unboxed as VU
import System.Environment

devCmd :: SubCmd
devCmd _ = do
  GAccount {inventory} <- Account.loadFromEnv
  experiment normalGemRecipes (getByItem inventory)

getByItem :: VU.Unbox a => VU.Vector a -> Item -> a
getByItem v i = VU.unsafeIndex v (fromEnum i)

main :: IO ()
main =
  getArgs >>= \case
    sc : args
      | Just handler <- lookup sc subCmdHandlers ->
        withArgs args (handler ("<prog> " <> sc))
    _ ->
      forM_ subCmdHandlers $ \(sub, _) ->
        putStrLn $ "<prog> " <> sub <> " ..."
  where
    subCmdHandlers =
      [ ("roll", Roll.subCmd)
      , ("_dev", devCmd)
      ]
