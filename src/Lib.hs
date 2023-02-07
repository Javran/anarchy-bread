module Lib (
  main,
) where

import Control.Monad.Cont
import Control.Monad.Writer.CPS
import Data.Function
import qualified Data.Map.Strict as M
import Data.Maybe
import System.Random.MWC

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

specialBreads = [Croissant, Flatbread, StuffedFlatbread, Sandwich, FrenchBread]
rareBreads = [Doughnut, Bagel, Waffle]

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

{-
  Regarding rolling one single item: it is done in the following order:

  - check for moak
  - check for gem, individually
    gold -> green -> purple -> blue -> red
  - check for chess pieces
  - check for rare breads
  - check for special breads
  - finally, just normal bread

  For a bread roll:
  - Lottery: 1/4096 chance, min(100, daily roll)
  - 10+:
    + first roll 1/512
      + fail: draw from [1..10]
      + success: at least 11, keep checking 1/2 until it fails.

 -}

getRollCount :: GenIO -> Account -> IO Int
getRollCount g Account {dailyRoll} =
  runContT (callCC rollCount) pure
  where
    rollCount k = do
      -- 1/4096 for a lottery
      lotto <- uniformR @Int (1, 4096) g
      when (lotto == 1) do
        k $ max 100 dailyRoll

      -- 1/512 for 11+
      ep <- uniformR @Int (1, 512) g
      when (ep == 1) do
        r <-
          fix
            ( \go cur -> do
                b <- uniformM @Bool g
                if b
                  then go (cur + 1)
                  else pure cur
            )
            11
        k r
      uniformR @Int (1, 10) g

testAccount :: Account
testAccount =
  Account
    { loafConverter = 256
    , dailyRoll = 1200
    , recipeRefinement = False
    , moakBooster = 0
    , chessPieceEqualizer = 1
    , etherealShine = 1
    , inventory = M.singleton (Shadow ShadowGemGold) 20
    , prestigeLevel = 2
    , gambitShop =
        M.fromList
          [ (Bread Flatbread, 2)
          , (Bread StuffedFlatbread, 2)
          , (Bread Sandwich, 2)
          , (Bread FrenchBread, 2)
          ]
    }

oneRoll :: GenIO -> Account -> IO (Item, Int)
oneRoll
  g
  Account
    { loafConverter
    , dailyRoll
    , moakBooster
    , chessPieceEqualizer
    , recipeRefinement
    , etherealShine
    , inventory
    , gambitShop
    } =
    runContT (callCC _roll) pure
    where
      applyLuckBoost flag base = if flag then 4 * (base - 1) + 1 else base
      _roll k = do
        (item, val) <- _rollBasic k
        let extra = fromMaybe 0 $ gambitShop M.!? item
        pure (item, val + extra)
      _rollBasic k = do
        -- rolling without taking into account ascension and gambit_shop
        -- Moak
        do
          let moakRarityMult = round @Double $ fromIntegral dailyRoll / 10
              moakLuck = round @Double $ fromIntegral (loafConverter + 1) * (1.3 ^ moakBooster)
          moak <- uniformR @Int (1, 32768 * moakRarityMult) g
          when (moak <= moakLuck) do
            k (ManyOfAKind, 2000 + dailyRoll * 10)
        do
          let countShadowGoldGem =
                fromMaybe 0 $ inventory M.!? Shadow ShadowGemGold
              gemBoost = min (etherealShine * 10) countShadowGoldGem
              gemLuck =
                applyLuckBoost recipeRefinement $ loafConverter + 1 + gemBoost
          -- Gems, individually
          forM_
            [ (GGold, 4194304, 5000)
            , (GGreen, 524288, 750)
            , (GPurple, 262144, 500)
            , (GBlue, 131072, 250)
            , (GRed, 65536, 150)
            ]
            \(c, hi, reward) -> do
              gem <- uniformR @Int (1, hi) g
              when (gem <= gemLuck) do
                k (Gem c, reward)
        let luck = applyLuckBoost recipeRefinement $ loafConverter + 1
        -- Chess pieces
        do
          cp <- uniformR @Int (1, 2048) g
          let whiteChance = case chessPieceEqualizer of
                0 -> 0.25
                1 -> 0.33
                2 -> 0.42
                3 -> 0.5
                _ -> error $ "unknown cpe: " <> show chessPieceEqualizer
          when (cp <= luck) do
            cr <- uniformR @Double (0, 1) g
            let (color, reward) = if cr <= whiteChance then (White, 80) else (Black, 40)
            p <- uniformR @Int (0, 15) g
            let piece
                  | p <= 7 = Pawn
                  | p <= 9 = Knight
                  | p <= 11 = Bishop
                  | p <= 13 = Rook
                  | p <= 14 = Queen
                  | otherwise = King
            k (ChessPiece color piece, reward)
        -- Rare breads
        rare <- uniformR @Int (1, 512) g
        when (rare <= luck) do
          i <- uniformR (0, length rareBreads - 1) g
          k (Bread $ rareBreads !! i, 10)
        -- Special breads
        spec <- uniformR @Int (1, 128) g
        when (spec <= luck) do
          i <- uniformR (0, length specialBreads - 1) g
          k (Bread $ specialBreads !! i, 5)
        pure (Bread Loaf, 1)

breadRoll g a@Account {prestigeLevel} = do
  n <- getRollCount g a
  (items, rewards) <- unzip <$> replicateM n (oneRoll g a)
  pure (items, round @Double @Int $ fromIntegral (sum rewards) * (1 + 0.1 * fromIntegral prestigeLevel))

main :: IO ()
main = do
  g <- createSystemRandom
  replicateM_ 100 do
    xs <- breadRoll g testAccount
    print xs
