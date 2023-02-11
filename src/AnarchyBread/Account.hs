module AnarchyBread.Account (
  GAccount (..),
  Account,
  ItemCount (..),
  DhallAccount,
) where

import AnarchyBread.Types
import Dhall
import qualified Data.Map.Strict as M

data GAccount i m = GAccount
  { dailyRoll :: i
  , loafConverter :: i
  , recipeRefinement :: Bool
  , moakBooster :: i
  , chessPieceEqualizer :: i
  , etherealShine :: i
  , inventory :: m
  , prestigeLevel :: i
  , gambitShop :: m
  }
  deriving (Functor, Show, Generic)

instance (FromDhall i, FromDhall m) => FromDhall (GAccount i m)

data ItemCount = ItemCount {item :: Text, count :: Natural}
  deriving (Generic, Show)

instance FromDhall ItemCount

type Account = GAccount Int (M.Map Item Int)

type DhallAccount = GAccount Natural [ItemCount]
