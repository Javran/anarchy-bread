module AnarchyBread.Account (
  Account (..),
  loadFromEnv,
) where

import qualified AnarchyBread.Account.Dhall as DA
import AnarchyBread.Emoji
import AnarchyBread.Types
import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Dhall
import System.Environment

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
  deriving (Show, Generic)

fromItemCounts :: [DA.ItemCount] -> (M.Map Item Int, [DA.ItemCount])
fromItemCounts = foldMap go
  where
    go ic@DA.ItemCount {DA.item = e, DA.count} = case emojiToEItem e of
      Left _ -> (mempty, [ic])
      Right i -> (M.singleton i (fromIntegral count), mempty)

loadDhallFromEnv :: IO DA.Account
loadDhallFromEnv =
  getEnv "ACCOUNT" >>= input auto . T.pack

fromDhallAccount :: DA.Account -> IO Account
fromDhallAccount
  DA.Account
    { DA.dailyRoll = dr
    , DA.loafConverter = lc
    , DA.recipeRefinement
    , DA.moakBooster = mb
    , DA.chessPieceEqualizer = cpe
    , DA.etherealShine = es
    , DA.inventory = iv
    , DA.prestigeLevel = pl
    , DA.gambitShop = gs
    } = do
    let convert xs = do
          let (ys, zs) = fromItemCounts xs
          unless (null zs) do
            putStrLn $ "Discarding unrecoginized ItemCounts: " <> show zs
          pure ys
    inventory <- convert iv
    gambitShop <- convert gs
    pure
      Account
        { dailyRoll = fromIntegral dr
        , loafConverter = fromIntegral lc
        , recipeRefinement
        , moakBooster = fromIntegral mb
        , chessPieceEqualizer = fromIntegral cpe
        , etherealShine = fromIntegral es
        , prestigeLevel = fromIntegral pl
        , gambitShop
        , inventory
        }

loadFromEnv :: IO Account
loadFromEnv = loadDhallFromEnv >>= fromDhallAccount
