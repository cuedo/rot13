
module Main
  ( main
  ) where

import Test.Hspec
import Test.QuickCheck

import Codec.Rot13

main :: IO ()
main = hspec $ do
  describe "Codec.Rot13" $ do

    it "correctly ciphers the alphabet" $ do
      rot13 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        `shouldBe` "NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm"

    it "is reversible" $
      property $ \s -> s == (rot13 . rot13) s
