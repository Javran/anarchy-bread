module AnarchyBread.Roll (
  subCmd,
) where

import AnarchyBread.Account as Account
import AnarchyBread.Types
import Control.Concurrent.Async
import Control.Monad.Cont
import Control.Monad.Writer.CPS
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Vector as V
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
                if b
                  then go (cur + 1)
                  else pure cur
            )
            11
        k r
      uniformR @Int (1, 10) g

{-
  TODO: general pattern is a range and a bound for testing generated random number.
  we can probably compute all of those without random process involved so to allow
  some more sharing (I suspect compiler is already floating stuff out, but
  it's better that we do it manually)
 -}
oneRoll :: GenIO -> Account -> IO (Item, Int)
oneRoll
  g
  GAccount
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
                0 -> 25
                1 -> 33
                2 -> 42
                3 -> 50
                _ -> error $ "unknown cpe: " <> show chessPieceEqualizer
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

breadRoll :: GenIO -> Account -> IO ([Item], Int)
breadRoll g a@GAccount {prestigeLevel} = do
  n <- getRollCount g a
  (items, rewards) <- unzip <$> replicateM n (oneRoll g a)
  pure
    ( items
    , round @Double @Int $ fromIntegral (sum rewards) * (1 + 0.1 * fromIntegral prestigeLevel)
    )

simulateRolls :: Int -> Int -> IO ()
simulateRolls n m = do
  account <- Account.loadFromEnv
  putStrLn "Using account config:"
  printer account
  let cnt = n * m
  printf "Rolling with %d threads x%d = %d times ...\n" n m cnt
  tot <-
    sum <$> do
      replicateConcurrently n do
        g <- createSystemRandom
        sum <$> replicateM m do
          (_, r) <- breadRoll g account
          pure r
  print @Double (fromIntegral tot / fromIntegral cnt)

subCmd :: SubCmd
subCmd cmdPrefix =
  getArgs >>= \case
    [] -> simulateRolls 16 65536
    ["dev"] -> timeIt do simulateRolls 1 65536
    [rawN, rawM]
      | [(n, "")] <- reads rawN
        , [(m, "")] <- reads rawM ->
        simulateRolls n m
    _ -> do
      putStrLn $ cmdPrefix <> ": rolling with preset config"
      putStrLn $ cmdPrefix <> " dev: rolling with a smaller count for fast development"
      putStrLn $ cmdPrefix <> " <threads> <rolls per thread>"
