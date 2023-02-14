module AnarchyBread.Parse (
  intP,
  itemP,
) where

import AnarchyBread.Emoji
import AnarchyBread.Types
import Control.Monad
import Data.Char
import qualified Data.Text as T
import Text.ParserCombinators.ReadP

intP :: (Integral i, Read i) => ReadP i
intP = read <$> munch1 isDigit

itemP :: ReadP Item
itemP = between (char ':') (char ':') do
  {-
    Ref: https://support.discord.com/hc/en-us/articles/360036479811-Custom-Emojis

    > Emoji names must be at least 2 characters long and can only contain alphanumeric characters and underscores
   -}
  raw <- munch1 (\ch -> ch == '_' || isDigit ch || isAsciiLower ch || isAsciiUpper ch)
  Right item <- pure $ emojiToEItem (T.pack raw)
  _ <- void (char '~' *> intP @Int) <++ pure ()
  pure item
