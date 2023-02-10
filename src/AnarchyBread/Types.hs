module AnarchyBread.Types (
  Item (..),
  ShadowItem (..),
  Bread (..),
  specialBreads,
  rareBreads,
  CColor (..),
  Piece (..),
  GColor (..),
  Account (..),
) where

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

data Item
  = Bread Bread
  | ChessPiece CColor Piece
  | Gem GColor
  | ManyOfAKind
  | Shadow ShadowItem
  deriving (Show, Eq, Ord)

data ShadowItem
  = ShadowOmega
  | ShadowGemGold
  | ShadowMoak
  deriving (Show, Eq, Ord)

data Bread
  = Loaf
  | Croissant
  | Flatbread
  | StuffedFlatbread
  | Sandwich
  | FrenchBread
  | Doughnut
  | Bagel
  | Waffle
  deriving (Show, Eq, Ord)

data CColor = Black | White
  deriving (Show, Eq, Ord)

data Piece = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Show, Eq, Ord)

data GColor
  = GRed
  | GBlue
  | GPurple
  | GGreen
  | GGold
  deriving (Show, Eq, Ord)

specialBreads :: V.Vector Bread
specialBreads = V.fromList [Croissant, Flatbread, StuffedFlatbread, Sandwich, FrenchBread]

rareBreads :: V.Vector Bread
rareBreads = V.fromList [Doughnut, Bagel, Waffle]

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
