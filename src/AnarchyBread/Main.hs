module AnarchyBread.Main (
  main,
) where

import AnarchyBread.Emoji
import qualified AnarchyBread.Roll as Roll
import AnarchyBread.Types
import Control.Monad
import System.Environment

devCmd :: SubCmd
devCmd _ = do
  mapM_ print mappings
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
