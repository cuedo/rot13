{-# LANGUAGE Trustworthy #-}

-- |
-- Module      : Codec.Rot13
-- Description : Fast ROT13 cipher for Haskell.
-- Copyright   : (c) Kyle Van Berendonck, 2014
-- License     : BSD3
-- Maintainer  : kvanberendonck@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module exposes all the API for this package.
module Codec.Rot13
  ( -- * Word
    rot13word
  , rot13word8
    -- * Char
  , rot13char
  , rot13
    -- * ByteString
  , rot13bs
  ) where

import           Data.Char
import           Data.Word
import qualified Data.ByteString as BS


-- | Perform the ROT13 cipher on the given ANSI encoded 'Word'.
{-# INLINE rot13word #-}
rot13word :: Word -> Word
rot13word x
  | x - 97 < 26 = 97 + rem (x - 84) 26
  | x - 65 < 26 = 65 + rem (x - 52) 26
  | otherwise   = x

-- | Like 'rot13word', but using the smaller 'Word8' type.
--
-- @
--    rot13word8 = fromIntegral . rot13word . fromIntegral
-- @
{-# INLINE rot13word8 #-}
rot13word8 :: Word8 -> Word8
rot13word8 = \x -> (fromIntegral . rot13word . fromIntegral) x

-- | Perform the ROT13 cipher on a single 'Char'. A ROT13 cipher is the inverse of itself, so one
--   function will both suitably encode and decode. Thus, the following holds:
--
-- @
--    rot13char . rot13char = id
-- @
{-# INLINE rot13char #-}
rot13char :: Char -> Char
rot13char = \x -> (chr . fromIntegral . rot13word . fromIntegral . ord) x

-- | Perform the ROT13 cipher on a 'String'. This is just equivalent to:
--
-- @
--    rot13 = map rot13char
-- @
{-# INLINABLE rot13 #-}
rot13 :: String -> String
rot13 = \s -> map rot13char s

-- | Like 'rot13' but for 'BS.ByteString's.
{-# INLINABLE rot13bs #-}
rot13bs :: BS.ByteString -> BS.ByteString
rot13bs = \s -> BS.map rot13word8 s
