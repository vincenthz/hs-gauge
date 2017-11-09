{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Gauge
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Core benchmarking code.

module Gauge
    (
    -- * Benchmarkable code
      Benchmarkable
    -- * Creating a benchmark suite
    , Benchmark
    , env
    , envWithCleanup
    , perBatchEnv
    , perBatchEnvWithCleanup
    , perRunEnv
    , perRunEnvWithCleanup
    , toBenchmarkable
    , bench
    , bgroup
    -- ** Running a benchmark
    , nf
    , whnf
    , nfIO
    , whnfIO
    -- * For interactive use
    , benchmark
    , benchmarkWith
    , benchmark'
    , benchmarkWith'
    ) where

import Gauge.Analysis (Report, analyseBenchmark)
import Gauge.Internal (quickAnalyse, runWithAnalysisInteractive)
import Gauge.Main.Options (defaultConfig)
import Gauge.Types

-- | Run a benchmark interactively, and analyse its performance.
benchmark :: Benchmarkable -> IO ()
benchmark = benchmarkWith defaultConfig

-- | Run a benchmark interactively, analyse its performance, and
-- return the analysis.
benchmark' :: Benchmarkable -> IO Report
benchmark' = benchmarkWith' defaultConfig

-- | Run a benchmark interactively, and analyse its performance.
benchmarkWith :: Config -> Benchmarkable -> IO ()
benchmarkWith = runWithAnalysisInteractive quickAnalyse

-- | Run a benchmark interactively, analyse its performance, and
-- return the analysis.
benchmarkWith' :: Config -> Benchmarkable -> IO Report
benchmarkWith' = runWithAnalysisInteractive analyseBenchmark
