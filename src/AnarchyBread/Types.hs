module AnarchyBread.Types (
  Item (..),
  ShadowItem (..),
  OneOfAKindItem (..),
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
  | Chessatron
  | OmegaChessatron
  | ManyOfAKind
  | Shadow ShadowItem
  | OneOfAKind OneOfAKindItem
  deriving (Show, Eq, Ord)

data ShadowItem
  = ShadowOmega
  | ShadowGemGold
  | ShadowMoak
  deriving (Show, Eq, Ord, Enum, Bounded)

data OneOfAKindItem
  = Anarchy
  | HolyHell
  | Horsey
  deriving (Show, Eq, Ord, Enum, Bounded)

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
  deriving (Show, Eq, Ord, Enum, Bounded)

data CColor = Black | White
  deriving (Show, Eq, Ord, Enum, Bounded)

data Piece = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Show, Eq, Ord, Enum, Bounded)

data GColor
  = GRed
  | GBlue
  | GPurple
  | GGreen
  | GGold
  deriving (Show, Eq, Ord, Enum, Bounded)

specialBreads :: V.Vector Bread
specialBreads =
  V.fromList
    [ Croissant
    , Flatbread
    , StuffedFlatbread
    , Sandwich
    , FrenchBread
    ]

rareBreads :: V.Vector Bread
rareBreads =
  V.fromList
    [ Doughnut
    , Bagel
    , Waffle
    ]

universe :: (Bounded a, Enum a) => [a]
universe = [minBound .. maxBound]

allItems :: V.Vector Item
allItems = V.fromList do
  fmap Bread universe
    <> chPieces
    <> fmap Gem universe
    <> [Chessatron, OmegaChessatron, ManyOfAKind]
    <> fmap Shadow universe
    <> fmap OneOfAKind universe
  where
    chPieces = do
      c <- universe @CColor
      pt <- universe @Piece
      pure $ ChessPiece c pt

itemIndices :: M.Map Item Int
itemIndices = M.fromList $ zip (V.toList allItems) [0 ..]

instance Enum Item where
  toEnum = (allItems V.!)
  fromEnum = (itemIndices M.!)

instance Bounded Item where
  minBound = V.head allItems
  maxBound = V.last allItems

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
