module AnarchyBread.Main (
  main,
) where

import qualified AnarchyBread.Roll as Roll
import AnarchyBread.Types
import Control.Monad
import System.Environment
import AnarchyBread.Recipe.Raw

devCmd :: SubCmd
devCmd _ = print allRecipes

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
