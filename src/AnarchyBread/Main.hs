module AnarchyBread.Main (
  main,
) where

import AnarchyBread.Emoji
import qualified AnarchyBread.Roll as Roll
import AnarchyBread.Types
import AnarchyBread.Account
import Control.Monad
import Dhall
import qualified Data.Text as T
import System.Environment

devCmd :: SubCmd
devCmd _ = do
  accountRaw <- getEnv "ACCOUNT"
  z <- input auto (T.pack accountRaw) :: IO DhallAccount
  print z
  pure ()

main :: IO ()
main =
  getArgs >>= \case
    sc : args
      | Just handler <- lookup sc subCmdHandlers ->
        withArgs args (handler sc)
    _ ->
      forM_ subCmdHandlers $ \(sub, _) ->
        putStrLn $ "<prog> " <> sub <> " ..."
  where
    subCmdHandlers =
      [ ("roll", Roll.subCmd)
      , ("_dev", devCmd)
      ]
