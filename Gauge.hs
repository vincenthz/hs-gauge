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

import Gauge.Analysis (benchmark', benchmarkWith')
import Gauge.Internal (benchmark, benchmarkWith)
import Gauge.Types
