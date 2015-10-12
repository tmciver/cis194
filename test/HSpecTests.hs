module Main where
 
import Test.Hspec
import Homework1
import Homework2.Log
import Homework2.LogAnalysis
import Homework3.Golf
 
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
    describe "Exercise 4" $ do
      describe "Test 'inOrder'" $ do
        it "Should create a list of LogMessage from a MessageTree" $ do
          let lm1 = LogMessage Info 3 "Test"
              lm2 = LogMessage Warning 5 "Test"
              lm3 = LogMessage (Error 5) 2 "Test"
              lm4 = LogMessage Warning 4 "Test"
              tree = Node (Node Leaf lm3 Leaf)
                     lm1
                     (Node (Node Leaf lm4 Leaf)
                      lm2
                      Leaf)
              expectedList = [lm3, lm1, lm4, lm2]
            in inOrder tree `shouldBe` expectedList
    describe "Exercise 5" $ do
      describe "Test 'whatWentWrong'" $ do
        it "Should convert a list of LogMessages to a list of String containing the message string of the LogMessages that have a severity of 50 or greater." $ do
           let log = [ "I 6 Completed armadillo processing"
                     , "I 1 Nothing to report"
                     , "E 99 10 Flange failed!"
                     , "I 4 Everything normal"
                     , "I 11 Initiating self-destruct sequence"
                     , "E 70 3 Way too many pickles"
                     , "E 65 8 Bad pickle-flange interaction detected"
                     , "W 5 Flange is due for a check-up"
                     , "I 7 Out for lunch, back in two time steps"
                     , "E 20 2 Too many pickles"
                     , "I 9 Back from lunch"
                     ]
               messages = map parseMessage log
               expectedResult = [ "Way too many pickles"
                                , "Bad pickle-flange interaction detected"
                                , "Flange failed!"
                                ]
             in whatWentWrong messages `shouldBe` expectedResult

-- Homework 3 Tests
  describe "Homework 3 Tests" $ do
    describe "Test `skips` function" $ do
      it "Should return `[\"ABCD\", \"BD\", \"C\", \"D\"]` when called with \"ABCD\"" $ do
        skips ("ABCD" :: String) `shouldBe` ["ABCD", "BD", "C", "D"]
      it "Should return `[\"hello!\", \"el!\", \"l!\", \"l\", \"o\", \"!\"]` when called with \"hello!\"" $ do
        skips ("hello!" :: String) `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
