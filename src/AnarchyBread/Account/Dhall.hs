module AnarchyBread.Account.Dhall (
  Account(..),
  ItemCount(..),
  ) where

import Dhall

data Account = Account
  { dailyRoll :: Natural
  , loafConverter :: Natural
  , recipeRefinement :: Bool
  , moakBooster :: Natural
  , chessPieceEqualizer :: Natural
  , etherealShine :: Natural
  , inventory :: [ItemCount]
  , prestigeLevel :: Natural
  , gambitShop :: [ItemCount]
  }
  deriving (Show, Generic)
instance FromDhall Account

data ItemCount = ItemCount {item :: Text, count :: Natural}
  deriving (Generic, Show)

instance FromDhall ItemCount
