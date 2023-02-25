module AnarchyBread.Parse (
  intP,
  itemP,
  rawEmojiP,
  eItemP,
) where

import AnarchyBread.Emoji
import AnarchyBread.Types
import Control.Monad
import Data.Bits
import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Text.ParserCombinators.ReadP

intP :: (Integral i, Read i) => ReadP i
intP = read <$> munch1 isDigit

itemP :: ReadP Item
itemP = do
  Right v <- eItemP
  pure v

eItemP :: ReadP EItem
eItemP = convert <$> rawEmojiP
  where
    convert = \case
      Left ch -> case unicodeEmojiItems M.!? ch of
        Just v -> Right v
        Nothing -> Left $ T.singleton ch
      Right r -> emojiToEItem r

mightBeEmoji :: Char -> Bool
mightBeEmoji ch = (ord ch .&. 0xFFF_000) == 0x1F_000

{-
  Discord emoji is a mess and there are few possible ways for the same emoji:

  - Unicode emoji (we will parse this as a single Char)
  - `:emoji_name:` or `:emoji_name~n:` where `n` is a number (can be ignored)
  - `<:emoji_name:m>` where `m` is a number (can be ignored)

  This function will either return a Char for a Unicode emoji,
  or extract that `emoji_name` part as a Text.

  Note that for our case this is good enough.
  But this is not meant to be an error-free parser since:

  - Unicode emoji could consist of multiple characters
  - Unicode emoji detection is a simple guess: we consider any codepoint that looks
    like 0x1F_XXXX an emoji.

 -}
rawEmojiP :: ReadP (Either Char T.Text)
rawEmojiP =
  (Left <$> soleCharEmojiP)
    <++ (Right <$> emojiP)
  where
    soleCharEmojiP = do
      ch <- satisfy mightBeEmoji
      next <- look
      case next of
        [] -> pure ch
        x : _ | isSpace x -> pure ch
        _ -> fail "not emoji"

    emojiP :: ReadP T.Text
    emojiP =
      between (char '<') (char '>') (eSimpP <* munch1 isDigit)
        <++ eSimpP

    eSimpP :: ReadP T.Text
    eSimpP = between (char ':') (char ':') do
      {-
        Ref: https://support.discord.com/hc/en-us/articles/360036479811-Custom-Emojis

        > Emoji names must be at least 2 characters long and can only contain alphanumeric characters and underscores
       -}
      raw <- munch1 (\ch -> ch == '_' || isDigit ch || isAsciiLower ch || isAsciiUpper ch)
      _ <- void (char '~' *> intP @Int) <++ pure ()
      pure $ T.pack raw
