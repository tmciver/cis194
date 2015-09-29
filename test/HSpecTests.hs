module Main where
 
import Test.Hspec
import Homework1
import Homework2.Log
import Homework2.LogAnalysis
 
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

    describe "Exercise 5" $ do
      describe "Towers of Hanoi" $ do
        it "2 discs has solution  [(\"a\",\"c\"), (\"a\",\"b\"), (\"c\",\"b\")]" $ do
          hanoi 2 "a" "b" "c" `shouldBe` [("a","c"), ("a","b"), ("c","b")]

-- Homework 2

  describe "Homework 2 tests" $ do
    describe "Exercise 1" $ do
      describe "Test `parseMessage`" $ do
        it "Should parse an error log message" $ do
          parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"
        it "Should parse an informational log message" $ do
          parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"
        it "Should parse an unknown log message" $ do
          parseMessage "This is not in the right format" `shouldBe` Unknown "This is not in the right format"
    describe "Exercise 2" $ do
      describe "Test `insert`" $ do
        it "Should return a single Node when inserting into an empty tree (Leaf)" $ do
          let logMessage = LogMessage Info 123 "Test Message"
          insert logMessage Leaf `shouldBe` Node Leaf logMessage Leaf
        it "Should not insert an Unknown LogMessage" $ do
          insert (Unknown "Bad message") Leaf `shouldBe` Leaf
        it "Should insert a LogMessage into the proper place in the given tree" $ do
          let lm = LogMessage Warning 4 "Test"
              t = Node (Node Leaf (LogMessage Info 3 "Test") Leaf)
                  (LogMessage Warning 5 "Test")
                  Leaf
              newTree = Node (Node
                              Leaf
                              (LogMessage Info 3 "Test")
                              (Node Leaf lm Leaf))
                        (LogMessage Warning 5 "Test")
                        Leaf
            in insert lm t `shouldBe` newTree
    describe "Exercise 3" $ do
      describe "Test 'build'" $ do
        it "Should convert a list of LogMessages to a MessageTree" $ do
          let lm1 = LogMessage Info 3 "Test"
              lm2 = LogMessage Warning 5 "Test"
              ml = [lm1, Unknown "Bad message", lm2]
              expectedTree = Node Leaf lm1 (Node Leaf lm2 Leaf)
            in build ml `shouldBe` expectedTree
