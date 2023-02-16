module AnarchyBread.Chessatron (
  subCmd,
) where

import AnarchyBread.Account as Account
import AnarchyBread.Emoji
import AnarchyBread.Recipe.Filter
import AnarchyBread.Recipe.Z3
import AnarchyBread.Types
import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import System.Environment
import System.Exit

getByItem :: VU.Unbox a => VU.Vector a -> Item -> a
getByItem v i = VU.unsafeIndex v (fromEnum i)

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

{-
  TODO: this is calculating from accounts only.
 -}
subCmd :: SubCmd
subCmd _cmdPrefix =
  getArgs >>= \case
    _ -> do
      GAccount {inventory} <- Account.loadFromEnv
      r <- maximizeItem Chessatron normalChessRecipes (getByItem inventory)
      case r of
        Nothing -> die "Solver failed"
        Just (x, y) -> pprResult x y
      pure ()
