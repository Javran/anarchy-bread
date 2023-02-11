module AnarchyBread.Account (
  Account (..),
) where

import AnarchyBread.Types

import qualified Data.Map.Strict as M

data Account = Account
  { dailyRoll :: Int
  , loafConverter :: Int
  , recipeRefinement :: Bool
  , moakBooster :: Int
  , chessPieceEqualizer :: Int
  , etherealShine :: Int
  , inventory :: M.Map Item Int
  , prestigeLevel :: Int
  , gambitShop :: M.Map Item Int
  }
