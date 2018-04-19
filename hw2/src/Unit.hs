module Unit
       ( hspecTestTree
       ) where

import Data.Char           (isLetter, toUpper, digitToInt, isDigit)
import Control.Applicative (liftA2, (<|>))

import Test.Tasty          (TestTree)
import Test.Tasty.Hspec    (Spec, describe, it, shouldBe, testSpec)

import Implementation

hspecTestTree :: IO TestTree
hspecTestTree = testSpec "Simple parser" spec_Parser

spec_Parser :: Spec
spec_Parser = do
  describe "evaluate works" $ do
    it "evaluate on negative power input" $
      evaluate (Pow (Number 2) (Number (-2))) `shouldBe` (Left RaisingToNegativePower)
    it "evaluate on division by zero input" $
      evaluate (Div (Number 15) (Number 0)) `shouldBe` (Left DivizionByZero)
    it "evaluate on non-error input" $
      evaluate (Sum (Mul (Number 2) (Number (4))) (Div (Pow (Number 10) (Number 2)) (Number 3))) `shouldBe` (Right 41)
  describe "stringSum works" $ do
    it "stringSum on correctTests" $
      traverse stringSum [ "1", "1 2 3", " 1", "1 ", "\t1\t", "\t12345\t", "010 020 030"
            , " 123 456 789 ", "-1", "-1 -2 -3", "\t-12345\t", " -123 -456 -789 "
            , "\n1\t\n3   555  -1\n\n\n-5", "123\t\n\t\n\t\n321 -4 -40"
            ] `shouldBe` Just [1, 6, 1, 1, 1, 12345, 60, 1368, -1, -6, -12345, -1368, 553, 400]
    it "stringSum on incorrectTests" $
      map stringSum ["asd", "1-1", "1.2", "--2", "+1", "1+"] `shouldBe` [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  describe "functor works" $ do
    it "functor on toUpper function" $
      map (runParser (pmany (toUpper <$> satisfy isLetter))) ["abacaba", "aaaa", "badadfa"] `shouldBe` (map (\x -> Just $ (map toUpper x, "")) ["abacaba", "aaaa", "badadfa"])
    it "functor on digitToInt function" $ 
      map (runParser (digitToInt <$> (satisfy isDigit))) ["a", "1f", "_2", " 3", "", "4"] `shouldBe` [Nothing, Just (1, "f"), Nothing , Nothing, Nothing, Just (4, "")]
  describe "applicative works" $ do
    it "applicative on sum" $
      map (runParser $ liftA2 (\x y -> digitToInt x + (digitToInt y)) (satisfy isDigit) (satisfy isDigit)) ["12", "123", "4", "456"] `shouldBe` [Just (3, ""), Just (3, "3"), Nothing, Just (9, "6")]
    it "applicative on words without leading digits" $ 
      map (runParser $ liftA2 (\_ y -> y) (pmany (satisfy isDigit)) (pmany (satisfy (\_ -> True)))) ["123Nikita", "Ivan", "124", "4ikolay"] `shouldBe` [Just ("Nikita", ""), Just ("Ivan", ""), Just ("", ""), Just ("ikolay", "")]
  describe "monad works" $ do
    it "monad on eof" $ 
      map (runParser $ (satisfy isDigit) >>= const eof) ["1", "2", "Many", "", "123"] `shouldBe` [Just ((), ""), Just ((), ""), Nothing, Nothing, Nothing]
    it "monad on all ones or all zeroes" $
      map (runParser $ (satisfy (\x -> elem x "01")) >>= \x -> (pmany (satisfy (== x))) >>= const eof) ["00000", "111111", "1", "0", "101", "010", ""] `shouldBe` [Just ((), ""), Just ((), ""), Just ((), ""), Just ((), ""), Nothing, Nothing, Nothing]
  describe "alternative" $ do
    it "alterative on all ones or all zeroes" $
      map (runParser $ ((pmany (satisfy (== '1'))) >>= const eof) <|> ((pmany (satisfy (== '0'))) >>= const eof)) ["00000", "111111", "1", "0", "101", "010", ""] `shouldBe` [Just ((), ""), Just ((), ""), Just ((), ""), Just ((), ""), Nothing, Nothing, Just ((), "")]
    it "alternative on starts on digit or eof" $
      map (runParser $ ((satisfy isDigit) >>= const ok) <|> eof) ["12", "1", " 1", ""] `shouldBe` [Just ((), "2"), Just ((), ""), Nothing, Just ((), "")]
  describe "eof works" $ do
    it "eof testing" $
      map (runParser eof) ["", "1", [], "1234"] `shouldBe` [Just ((), ""), Nothing, Just ((), ""), Nothing]
  describe "ok works" $ do
    it "ok testing" $
      map (runParser ok) ["", " ", "\n", "1234"] `shouldBe` [Just ((), ""), Just ((), " "), Just ((), "\n"), Just ((), "1234")]
  describe "satisfy works" $ do
    it "satisfy testing" $
      (map runParser [satisfy isDigit, satisfy (\_ -> True), satisfy (\_ -> False)]) <*> ["11", "asdf"] `shouldBe` [Just ('1', "1"), Nothing, Just ('1', "1"), Just ('a', "sdf"), Nothing, Nothing]
  describe "element works" $ do
    it "element testing" $
      (map runParser [element '0', element 'N']) <*> ["01", "Nikita"] `shouldBe` [Just ('0', "1"), Nothing, Nothing, Just ('N', "ikita")]
  describe "stream works" $ do
    it "stream testing" $ 
      (map runParser [stream "Ivan", stream "Nikolay", stream "Nikita"]) <*> ["IvanIvan", "NikolayNikolay", "NikitaNikita"] `shouldBe` [Just ("Ivan", "Ivan"), Nothing, Nothing, Nothing, Just ("Nikolay", "Nikolay"), Nothing, Nothing, Nothing, Just ("Nikita", "Nikita")]
  describe "parenthesis works" $ do
    it "parenthesis on correct tests" $
      map (runParser parenthesis) ["()", "", "()()", "(())()"] `shouldBe` (replicate 4 (Just ((), "")))
    it "parenthesis on incorrect tests" $
      map (runParser parenthesis) ["() ", " ()", "(", ")", "(()", "())", "(( ))"] `shouldBe` (replicate 7 Nothing)
  describe "pinteger works" $ do
    it "pinteger on correct tests" $ 
      map (runParser pinteger) ["+0", "-0", "0", "1234", "123", "-1234", "-123", "9", "-9"] `shouldBe` (map (\x -> Just (x, "")) [0, 0, 0, 1234, 123, -1234, -123, 9, -9])
    it "pinteger on incorrent tests" $
      map (runParser pinteger) [" +0", "0-", " ", "12 34", "--123", "++1234", "-", "+"] `shouldBe` (replicate 8 Nothing)
  describe "listOflist works" $ do
    it "listOflist correct tests" $
      map (runParser listOflist) ["2, 1,+10  , 3,5,-7, 2", "0", "0,0,0", "1, -1, 1, -1", "      1   ,       0"] `shouldBe` (map (\x -> Just (x, "")) [[ [1, 10], [5, -7, 2] ], [[]], [[], [], []], [[-1], [-1]], [[0]]])
    it "listOflist incorrect tests" $ 
      map (runParser listOflist) ["1 ", "", "0, 1, 1, 1", "         "] `shouldBe` (replicate 4 Nothing)