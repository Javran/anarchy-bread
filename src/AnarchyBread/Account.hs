{-# LANGUAGE TemplateHaskell #-}

module AnarchyBread.Account (
  GAccount (..),
  Account,
  ItemCount (..),
  DhallAccount,
  fromEnv,
) where

import AnarchyBread.Emoji
import AnarchyBread.Types
import Control.Monad
import Data.Bifunctor
import Data.Bifunctor.TH
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Dhall
import System.Environment

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

type Account = GAccount Int (M.Map Item Int)

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
  pure $ first fromIntegral ga1

fromEnv :: IO Account
fromEnv = loadDhallFromEnv >>= fromDhallAccount
