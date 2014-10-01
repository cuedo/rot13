
module Codec.Rot13
  ( rot13char
  , rot13
  ) where

import Data.Char
import Data.Word

{-# INLINE rot13char #-}
rot13char :: Char -> Char
rot13char = \x -> (chr . fromIntegral . transform . fromIntegral . ord) x
  where
    transform :: Word -> Word
    transform x
      | x - 97 < 26 = 97 + rem (x - 84) 26
      | x - 65 < 26 = 65 + rem (x - 52) 26
      | otherwise   = x

{-# INLINE rot13 #-}
rot13 :: String -> String
rot13 = \s -> map rot13char s
