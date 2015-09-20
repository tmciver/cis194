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

  describe "Validate doubleEveryOther function" $ do
    it "doubleEveryOther is supposed to double every other Int from the input list starting with the last (meaning it will double the second-to-last Int" $ do
      doubleEveryOther [4, 3, 2, 1] `shouldBe` [8, 3, 4, 1]

  describe "Validate sumDigits function" $ do
    it "sumDigits calculates the sum of the Ints in the input list" $ do
      sumDigits [8, 3, 4, 1] `shouldBe` 16
