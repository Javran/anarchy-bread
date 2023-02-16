module AnarchyBread.Types (
  universe,
  Item (..),
  ShadowItem (..),
  OneOfAKindItem (..),
  Bread (..),
  specialBreads,
  rareBreads,
  CColor (..),
  Piece (..),
  GColor (..),
  StonkType (..),
  SubCmdContext,
  SubCmd,
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
  | Stonk StonkType
  deriving (Show, Eq, Ord)

data StonkType
  = Cookie
  | Pretzel
  | FortuneCookie
  deriving (Show, Eq, Ord, Enum, Bounded)

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

{-
  Gem colors, ordered from most common one to rarest.
 -}
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
    <> fmap Stonk universe
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

type SubCmdContext = String

type SubCmd = SubCmdContext -> IO ()
