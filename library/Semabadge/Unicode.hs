module Semabadge.Unicode
  ( fromUtf8
  , toUtf8
  ) where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

fromUtf8 :: ByteString.ByteString -> String
fromUtf8 byteString =
  Text.unpack (Text.decodeUtf8With Text.lenientDecode byteString)

toUtf8 :: String -> ByteString.ByteString
toUtf8 string = Text.encodeUtf8 (Text.pack string)
