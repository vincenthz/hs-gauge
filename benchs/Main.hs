module Main where

import Gauge.Main

main = defaultMain
    [ bench "identity" $ whnf (map (+ 1)) [1,2,3 :: Int]
    ]
