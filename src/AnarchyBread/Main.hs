module AnarchyBread.Main (
  main,
) where

import qualified AnarchyBread.Gold as Gold
import qualified AnarchyBread.Roll as Roll
import qualified AnarchyBread.Chessatron as Chess
import AnarchyBread.Types
import Control.Monad
import System.Environment

devCmd :: SubCmd
devCmd _ = pure ()

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
      , ("gold", Gold.subCmd)
      , ("chess", Chess.subCmd)
      , ("_dev", devCmd)
      ]
