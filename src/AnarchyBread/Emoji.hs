module AnarchyBread.Emoji (
  EItem,
  mappings,
  itemToEmoji,
  emojiToEItem,
) where

import AnarchyBread.Types
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Text as T

type EItem = Either T.Text Item

{-
  Mappings between Discord emojis and items.
 -}
mappings :: [(T.Text, Item)]
mappings =
  [ ("bread", Bread Loaf)
  , ("croissant", Bread Croissant)
  , ("flatbread", Bread Flatbread)
  , ("stuffed_flatbread", Bread StuffedFlatbread)
  , ("sandwich", Bread Sandwich)
  , ("french_bread", Bread FrenchBread)
  , ("doughnut", Bread Doughnut)
  , ("bagel", Bread Bagel)
  , ("waffle", Bread Waffle)
  , ("chessatron", Chessatron)
  , ("omega_chessatron", OmegaChessatron)
  , ("anarchy_chess", ManyOfAKind)
  , ("shadowmega_chessatron", Shadow ShadowOmega)
  , ("shadow_gem_gold", Shadow ShadowGemGold)
  , ("shadow_moak", Shadow ShadowMoak)
  , ("anarchy", OneOfAKind Anarchy)
  , ("holy_hell", OneOfAKind HolyHell)
  , ("horsey", OneOfAKind Horsey)
  ]
    <> pMappings
    <> gMappings
  where
    pMappings = do
      (c, ct) <- [(Black, 'b'), (White, 'w')]
      (p, pt) <-
        [ (Pawn, "pawn")
          , (Knight, "knight")
          , (Bishop, "bishop")
          , (Rook, "rook")
          , (Queen, "queen")
          , (King, "king")
          ]
      pure (T.cons ct pt, ChessPiece c p)
    gMappings = do
      (c, ct) <-
        [ (GRed, "red")
          , (GBlue, "blue")
          , (GPurple, "purple")
          , (GGreen, "green")
          , (GGold, "gold")
          ]
      pure ("gem_" <> ct, Gem c)

dItemToEmoji :: IM.IntMap T.Text
dItemToEmoji =
  IM.fromListWith
    (error "duplicated keys in item -> emoji mappings")
    $ fmap (\(e, i) -> (fromEnum i, e)) mappings

dEmojiToItem :: M.Map T.Text Item
dEmojiToItem =
  M.fromListWith
    (error "duplicated keys in emoji -> item mappings")
    mappings

itemToEmoji :: Item -> T.Text
itemToEmoji = (dItemToEmoji IM.!) . fromEnum

emojiToEItem :: T.Text -> EItem
emojiToEItem raw =
  maybe (Left raw) Right (dEmojiToItem M.!? raw)
