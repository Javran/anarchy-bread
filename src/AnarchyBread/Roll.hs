{-# LANGUAGE RecordWildCards #-}

module AnarchyBread.Roll (
  subCmd,
) where

import AnarchyBread.Account as Account
import AnarchyBread.Emoji (itemToEmoji)
import AnarchyBread.Types
import Control.Concurrent.Async
import Control.Monad.Cont
import Control.Monad.Writer.CPS
import Data.List
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Shower
import System.Environment
import System.Random.MWC
import System.TimeIt (timeIt)
import Text.Printf

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
getRollCount g GAccount {dailyRoll} =
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
                {-
                  11+ won't go beyond 99, presumably just a simple way
                  to distinguish between lotto and 11+.
                 -}
                if b && cur < 99
                  then go (cur + 1)
                  else pure cur
            )
            11
        k r
      uniformR @Int (1, 10) g

data RollPrecompute = RollPrecompute
  { moakRarityMult :: Int
  , moakLuck :: Int
  , gemLuck :: Int
  , luck :: Int
  , whiteChance :: Int
  }

getByItem :: VU.Unbox a => VU.Vector a -> Item -> a
getByItem v i = VU.unsafeIndex v (fromEnum i)

precompute :: Account -> RollPrecompute
precompute
  GAccount
    { loafConverter
    , dailyRoll
    , moakBooster
    , recipeRefinement
    , chessPieceEqualizer
    , etherealShine
    , inventory
    } =
    RollPrecompute {..}
    where
      applyLuckBoost flag base = if flag then 4 * (base - 1) + 1 else base
      moakRarityMult = round @Double $ fromIntegral dailyRoll / 10
      moakLuck = round @Double $ fromIntegral (loafConverter + 1) * (1.3 ^ moakBooster)
      countShadowGoldGem =
        getByItem inventory $ Shadow ShadowGemGold
      gemBoost = min (etherealShine * 10) countShadowGoldGem
      gemLuck =
        applyLuckBoost recipeRefinement $ loafConverter + 1 + gemBoost
      luck = applyLuckBoost recipeRefinement $ loafConverter + 1
      whiteChance = case chessPieceEqualizer of
        0 -> 25
        1 -> 33
        2 -> 42
        3 -> 50
        _ -> error $ "unknown cpe: " <> show chessPieceEqualizer

applyLotto :: RollPrecompute -> RollPrecompute
applyLotto rp@RollPrecompute {moakLuck, gemLuck, luck} =
  rp
    { moakLuck = moakLuck * 4
    , gemLuck = gemLuck * 4
    , luck = luck * 4
    }

oneRoll :: GenIO -> RollPrecompute -> Account -> IO (Item, Int)
oneRoll
  g
  RollPrecompute {..}
  GAccount
    { dailyRoll
    , gambitShop
    } =
    runContT (callCC _roll) pure
    where
      _roll k = do
        (item, val) <- _rollBasic k
        let extra = getByItem gambitShop item
        pure (item, val + extra)
      _rollBasic k = do
        -- rolling without taking into account ascension and gambit_shop
        -- Moak
        do
          moak <- uniformR @Int (1, 32768 * moakRarityMult) g
          when (moak <= moakLuck) do
            k (ManyOfAKind, 2000 + dailyRoll * 10)
        do
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
        -- Chess pieces
        do
          cp <- uniformR @Int (1, 2048) g
          when (cp <= luck) do
            cr <- uniformR @Int (0, 99) g
            let (color, reward) = if cr < whiteChance then (White, 80) else (Black, 40)
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
          k (Bread $ rareBreads V.! i, 10)
        -- Special breads
        spec <- uniformR @Int (1, 128) g
        when (spec <= luck) do
          i <- uniformR (0, length specialBreads - 1) g
          k (Bread $ specialBreads V.! i, 5)
        pure (Bread Loaf, 1)

breadRoll :: GenIO -> RollPrecompute -> Account -> IO ([Item], Int)
breadRoll g preNorm a@GAccount {prestigeLevel} = do
  let preLotto = applyLotto preNorm
  n <- getRollCount g a
  let pre = if n >= 100 then preLotto else preNorm
  (items, rewards) <- unzip <$> replicateM n (oneRoll g pre a)
  pure
    ( items
    , round @Double @Int $ fromIntegral (sum rewards) * (1 + 0.1 * fromIntegral prestigeLevel)
    )

newtype ItemVec = ItemVec (VU.Vector Int)

instance Show ItemVec where
  show (ItemVec vs) = "[" <> intercalate "," (fmap ppr pairs) <> "]"
    where
      pairs :: [(Item, Int)]
      pairs = filter ((/= 0) . snd) $ zip universe (VU.toList vs)
      ppr (item, v) = (T.unpack . itemToEmoji $ item) <> " x" <> show v

simulateRolls :: Int -> Int -> IO ()
simulateRolls n m = do
  account <- Account.loadFromEnv
  putStrLn "Using account config:"
  printer (fmap ItemVec account)
  let cnt = n * m
      pre = precompute account
  printf "Rolling with %d threads x%d = %d times ...\n" n m cnt
  tot <-
    sum <$> do
      replicateConcurrently n do
        g <- createSystemRandom
        sum <$> replicateM m do
          (_, r) <- breadRoll g pre account
          pure r
  print @Double (fromIntegral tot / fromIntegral cnt)

subCmd :: SubCmd
subCmd cmdPrefix =
  getArgs >>= \case
    [] -> simulateRolls 16 262144
    ["dev"] -> timeIt do simulateRolls 1 65536
    [rawN, rawM]
      | [(n, "")] <- reads rawN
        , [(m, "")] <- reads rawM ->
        simulateRolls n m
    _ -> do
      putStrLn $ cmdPrefix <> ": rolling with preset config"
      putStrLn $ cmdPrefix <> " dev: rolling with a smaller count for fast development"
      putStrLn $ cmdPrefix <> " <threads> <rolls per thread>"
