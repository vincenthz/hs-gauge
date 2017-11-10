{-# LANGUAGE CPP         #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- |
-- Module      : Gauge.Types
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Types for benchmarking.
--
-- The core type is 'Benchmarkable', which admits both pure functions
-- and 'IO' actions.
--
-- For a pure function of type @a -> b@, the benchmarking harness
-- calls this function repeatedly, each time with a different 'Int64'
-- argument (the number of times to run the function in a loop), and
-- reduces the result the function returns to weak head normal form.
--
-- For an action of type @IO a@, the benchmarking harness calls the
-- action repeatedly, but does not reduce the result.

module Gauge.Types
    (
    -- * Configuration
      Config(..)
    , Mode(..)
    , DisplayMode(..)
    , MatchType(..)
    , Verbosity(..)
    ) where

import Data.Data (Data, Typeable)
import Data.Int (Int64)
import GHC.Generics (Generic)
import Prelude

-- | Control the amount of information displayed.
data Verbosity = Quiet
               | Normal
               | Verbose
                 deriving (Eq, Ord, Bounded, Enum, Read, Show, Typeable, Data,
                           Generic)

-- | How to match a benchmark name.
data MatchType = Prefix
                 -- ^ Match by prefix. For example, a prefix of
                 -- @\"foo\"@ will match @\"foobar\"@.
               | Pattern
                 -- ^ Match by searching given substring in benchmark
                 -- paths.
               | IPattern
                 -- ^ Same as 'Pattern', but case insensitive.
               deriving (Eq, Ord, Bounded, Enum, Read, Show, Typeable, Data,
                         Generic)

-- | Execution mode for a benchmark program.
data Mode = List
            -- ^ List all benchmarks.
          | Version
            -- ^ Print the version.
          | Help
            -- ^ Print help
          | DefaultMode
            -- ^ Default Benchmark mode
          deriving (Eq, Read, Show, Typeable, Data, Generic)

data DisplayMode =
      Condensed
    | StatsTable
    deriving (Eq, Read, Show, Typeable, Data, Generic)

-- | Top-level benchmarking configuration.
data Config = Config {
      confInterval :: Maybe Double
      -- ^ Confidence interval for bootstrap estimation (greater than
      -- 0, less than 1).
    , forceGC      :: Bool
      -- ^ /Obsolete, unused/.  This option used to force garbage
      -- collection between every benchmark run, but it no longer has
      -- an effect (we now unconditionally force garbage collection).
      -- This option remains solely for backwards API compatibility.
    , timeLimit    :: Double
      -- ^ Number of seconds to run a single benchmark.  (In practice,
      -- execution time will very slightly exceed this limit.)
    , quickMode    :: Bool
    -- ^ Quickly measure and report raw measurements.
    , measureOnly  :: Maybe FilePath
    -- ^ Just measure the given benchmark and place the raw output in this
    -- file, do not analyse and generate a report.
    , measureWith  :: Maybe FilePath
    -- ^ Specify the path of the benchmarking program to use (this program
    -- itself) for measuring the benchmarks in a separate process.
    , resamples    :: Int
      -- ^ Number of resamples to perform when bootstrapping.
    , regressions  :: [([String], String)]
      -- ^ Regressions to perform.
    , rawDataFile  :: Maybe FilePath
      -- ^ File to write binary measurement and analysis data to.  If
      -- not specified, this will be a temporary file.
    , reportFile   :: Maybe FilePath
      -- ^ File to write report output to, with template expanded.
    , csvFile      :: Maybe FilePath
      -- ^ File to write CSV summary to.
    , jsonFile     :: Maybe FilePath
      -- ^ File to write JSON-formatted results to.
    , junitFile    :: Maybe FilePath
      -- ^ File to write JUnit-compatible XML results to.
    , verbosity    :: Verbosity
      -- ^ Verbosity level to use when running and analysing
      -- benchmarks.
    , template     :: FilePath
      -- ^ Template file to use if writing a report.
    , iters        :: Maybe Int64
      -- ^ Number of iterations
    , match        :: MatchType
      -- ^ Type of matching to use, if any
    , mode         :: Mode
      -- ^ Mode of operation
    , displayMode  :: DisplayMode
    } deriving (Eq, Read, Show, Typeable, Data, Generic)
