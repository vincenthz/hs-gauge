module Main (main) where

import Properties
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Tests"
       [ Properties.tests
       ]
