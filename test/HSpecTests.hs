module Main where
 
import Test.Hspec
import Homework1
 
main :: IO ()
main = hspec $ do

  describe "Homework 1 Tests" $ do
    describe "Exercise 1" $ do
      describe "Validate toDigits function" $ do
         it "toDigits is supposed to transform an Integer to a list of digits" $ do
           toDigits 1234 `shouldBe` [1, 2, 3, 4]

      describe "Validate toDigitsRev function" $ do
        it "toDigitsRev is supposed to transform an Integer to a list of digits in reverse order" $ do
          toDigitsRev 1234 `shouldBe` [4, 3, 2, 1]

    describe "Exercise 2" $ do
      describe "Validate doubleEveryOther function" $ do
        it "doubleEveryOther is supposed to double every other Integer from the input list starting with the second Integer" $ do
          doubleEveryOther [4, 3, 2, 1] `shouldBe` [8, 3, 4, 1]

      describe "Validate doubleEveryOther function" $ do
        it "doubleEveryOther is supposed to double every other Integer from the input list starting with the second Integer" $ do
          doubleEveryOther [1, 2, 3] `shouldBe` [1, 4, 3]

    describe "Exercise 3" $ do
      describe "Validate sumDigits function" $ do
        it "sumDigits calculates the sum of the Ints in the input list" $ do
          sumDigits [16,7,12,5] `shouldBe` 22

    describe "Exercise 4" $ do
      describe "Check that validate returns False for 4012888888881882" $ do
        it "validate returns True for Ints that are evenly divisible by 10, False otherwise." $ do
          validate 4012888888881882 `shouldBe` False

      describe "Check that validate returns True for 4012888888881881" $ do
        it "validate returns True for Ints that are evenly divisible by 10, False otherwise." $ do
          validate 4012888888881881 `shouldBe` True
