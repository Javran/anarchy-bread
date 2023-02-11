{-# LANGUAGE TemplateHaskell #-}

module AnarchyBread.Account (
  GAccount (..),
  Account,
  ItemCount (..),
  DhallAccount,
  loadFromEnv,
) where

import AnarchyBread.Emoji
import AnarchyBread.Types
import Control.Monad
import Data.Bifunctor
import Data.Bifunctor.TH
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import Dhall
import System.Environment
import Data.Maybe

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
  deriving (Functor, Show, Generic, Foldable, Traversable)

$(deriveBifunctor ''GAccount)

instance (FromDhall i, FromDhall m) => FromDhall (GAccount i m)

data ItemCount = ItemCount {item :: Text, count :: Natural}
  deriving (Generic, Show)

instance FromDhall ItemCount

packFromMap :: M.Map Item Int -> VU.Vector Int
packFromMap m = VU.generate (1 + fromEnum (maxBound :: Item)) (fromMaybe 0 . (m M.!?) . toEnum @Item)

type Account = GAccount Int (VU.Vector Int)

type DhallAccount = GAccount Natural [ItemCount]

fromItemCounts :: [ItemCount] -> (M.Map Item Int, [ItemCount])
fromItemCounts = foldMap go
  where
    go ic@ItemCount {item = e, count} = case emojiToEItem e of
      Left _ -> (mempty, [ic])
      Right i -> (M.singleton i (fromIntegral count), mempty)

loadDhallFromEnv :: IO DhallAccount
loadDhallFromEnv =
  getEnv "ACCOUNT" >>= input auto . T.pack

fromDhallAccount :: DhallAccount -> IO Account
fromDhallAccount ga0 = do
  ga1 <-
    traverse
      ( \xs -> do
          let (ys, zs) = fromItemCounts xs
          unless (null zs) do
            putStrLn $ "Discarding unrecoginized ItemCounts: " <> show zs
          pure ys
      )
      ga0
  pure $ bimap fromIntegral packFromMap ga1

loadFromEnv :: IO Account
loadFromEnv = loadDhallFromEnv >>= fromDhallAccount
