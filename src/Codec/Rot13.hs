
module Codec.Rot13
  ( rot13word
  , rot13char
  , rot13
  , rot13bs
  ) where

import           Data.Char
import           Data.Word
import qualified Data.ByteString as BS


{-# INLINE rot13word #-}
rot13word :: Word -> Word
rot13word x
  | x - 97 < 26 = 97 + rem (x - 84) 26
  | x - 65 < 26 = 65 + rem (x - 52) 26
  | otherwise   = x

{-# INLINE rot13word8 #-}
rot13word8 :: Word8 -> Word8
rot13word8 = \x -> (fromIntegral . rot13word . fromIntegral) x

{-# INLINE rot13char #-}
rot13char :: Char -> Char
rot13char = \x -> (chr . fromIntegral . rot13word . fromIntegral . ord) x

{-# INLINABLE rot13bs #-}
rot13bs :: BS.ByteString -> BS.ByteString
rot13bs = \s -> BS.map rot13word8 s

{-# INLINABLE rot13 #-}
rot13 :: String -> String
rot13 = \s -> map rot13char s
