module Main (main) where

import Gauge.Types
import qualified Data.Vector as V
import Properties
import Statistics.Types (estimateFromErr, mkCL)
import Test.Tasty (defaultMain, testGroup)
import Test.HUnit

main :: IO ()
main = defaultMain $ testGroup "Tests"
       [ Properties.tests
       ]
