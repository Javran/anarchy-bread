module AnarchyBread.Main (
  main,
) where

import qualified AnarchyBread.Roll as Roll
import AnarchyBread.Recipe.Z3
import AnarchyBread.Recipe.Filter
import AnarchyBread.Types
import Control.Monad
import System.Environment

devCmd :: SubCmd
devCmd _ = experiment normalGemRecipes

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
