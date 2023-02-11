module AnarchyBread.Account (
  GAccount (..),
  Account,
) where

import AnarchyBread.Types

import qualified Data.Map.Strict as M

data GAccount m = GAccount
  { dailyRoll :: Int
  , loafConverter :: Int
  , recipeRefinement :: Bool
  , moakBooster :: Int
  , chessPieceEqualizer :: Int
  , etherealShine :: Int
  , inventory :: m
  , prestigeLevel :: Int
  , gambitShop :: m
  }
  deriving (Functor, Show)

type Account = GAccount (M.Map Item Int)
