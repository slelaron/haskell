module Main where

import Test.Tasty (defaultMain, testGroup)

import Unit (hspecTestTree)

main :: IO ()
main = hspecTestTree >>= \unitTests ->
       let allTests = testGroup "Parser" [unitTests]
       in defaultMain allTests