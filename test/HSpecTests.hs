module Main where
 
import Test.Hspec
import Homework1
 
main :: IO ()
main = hspec $ do
 
  describe "Validate toDigits function" $ do
    it "toDigits is supposed to transform an Integer to a list of digits" $ do
      toDigits 1234 `shouldBe` [1, 2, 3, 4]

  describe "Validate toDigitsRev function" $ do
    it "toDigitsRev is supposed to transform an Integer to a list of digits in reverse order" $ do
      toDigitsRev 1234 `shouldBe` [4, 3, 2, 1]
