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
    -- * Measurements
    , Measured(..)
    , fromInt
    , toInt
    , fromDouble
    , toDouble
    , measureAccessors
    , measureAccessors_
    , validateAccessors
    , renderNames
    , measureKeys
    , rescale
    , secs
    ) where

-- Temporary: to support pre-AMP GHC 7.8.4:
import Control.Applicative

import Control.DeepSeq (NFData(rnf))
import Control.Monad (when, unless)
import Data.Data (Data, Typeable)
import Data.Int (Int64)
import qualified Data.List as List
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Text.Printf (printf)
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

-- | A collection of measurements made while benchmarking.
--
-- Measurements related to garbage collection are tagged with __GC__.
-- They will only be available if a benchmark is run with @\"+RTS
-- -T\"@.
--
-- __Packed storage.__ When GC statistics cannot be collected, GC
-- values will be set to huge negative values.  If a field is labeled
-- with \"__GC__\" below, use 'fromInt' and 'fromDouble' to safely
-- convert to \"real\" values.
data Measured = Measured {
      measIters              :: !Int64
      -- ^ Number of loop iterations measured.
    , measTime               :: !Double
      -- ^ Total wall-clock time elapsed, in seconds.
    , measCycles             :: !Int64
      -- ^ Cycles, in unspecified units that may be CPU cycles.  (On
      -- i386 and x86_64, this is measured using the @rdtsc@
      -- instruction.)
    , measCpuTime            :: !Double
      -- ^ Total CPU time elapsed, in seconds.  Includes both user and
      -- kernel (system) time.

    , measUtime              :: !Int64
    -- ^ User time
    , measStime              :: !Int64
    -- ^ System time
    , measMaxrss             :: !Int64
    -- ^ Maximum resident set size
    , measMinflt             :: !Int64
    -- ^ Minor page faults
    , measMajflt             :: !Int64
    -- ^ Major page faults
    , measNvcsw              :: !Int64
    -- ^ Number of voluntary context switches
    , measNivcsw             :: !Int64
    -- ^ Number of involuntary context switches

    , measAllocated          :: !Int64
      -- ^ __(GC)__ Number of bytes allocated.  Access using 'fromInt'.
    , measNumGcs             :: !Int64
      -- ^ __(GC)__ Number of garbage collections performed.  Access
      -- using 'fromInt'.
    , measBytesCopied        :: !Int64
      -- ^ __(GC)__ Number of bytes copied during garbage collection.
      -- Access using 'fromInt'.
    , measMutatorWallSeconds :: !Double
      -- ^ __(GC)__ Wall-clock time spent doing real work
      -- (\"mutation\"), as distinct from garbage collection.  Access
      -- using 'fromDouble'.
    , measMutatorCpuSeconds  :: !Double
      -- ^ __(GC)__ CPU time spent doing real work (\"mutation\"), as
      -- distinct from garbage collection.  Access using 'fromDouble'.
    , measGcWallSeconds      :: !Double
      -- ^ __(GC)__ Wall-clock time spent doing garbage collection.
      -- Access using 'fromDouble'.
    , measGcCpuSeconds       :: !Double
      -- ^ __(GC)__ CPU time spent doing garbage collection.  Access
      -- using 'fromDouble'.
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance NFData Measured where
    rnf Measured{} = ()

-- | Convert a number of seconds to a string.  The string will consist
-- of four decimal places, followed by a short description of the time
-- units.
secs :: Double -> String
secs k
    | k < 0      = '-' : secs (-k)
    | k >= 1     = k        `with` "s"
    | k >= 1e-3  = (k*1e3)  `with` "ms"
#ifdef mingw32_HOST_OS
    | k >= 1e-6  = (k*1e6)  `with` "us"
#else
    | k >= 1e-6  = (k*1e6)  `with` "μs"
#endif
    | k >= 1e-9  = (k*1e9)  `with` "ns"
    | k >= 1e-12 = (k*1e12) `with` "ps"
    | k >= 1e-15 = (k*1e15) `with` "fs"
    | k >= 1e-18 = (k*1e18) `with` "as"
    | otherwise  = printf "%g s" k
     where with (t :: Double) (u :: String)
               | t >= 1e9  = printf "%.4g %s" t u
               | t >= 1e3  = printf "%.0f %s" t u
               | t >= 1e2  = printf "%.1f %s" t u
               | t >= 1e1  = printf "%.2f %s" t u
               | otherwise = printf "%.3f %s" t u

-- THIS MUST REFLECT THE ORDER OF FIELDS IN THE DATA TYPE.
--
-- The ordering is used by Javascript code to pick out the correct
-- index into the vector that represents a Measured value in that
-- world.
measureAccessors_ :: [(String, (Measured -> Maybe Double
                               , Double -> String
                               , String)
                     )]
measureAccessors_ = [
    ("iters",              ( Just . fromIntegral . measIters
                           , show . rnd
                           , "loop iterations"))
  , ("time",               ( Just . measTime
                           , secs
                           , "wall-clock time"))
  , ("cycles",             ( Just . fromIntegral . measCycles
                           , show . rnd
                           , "CPU cycles"))
  , ("cpuTime",            ( Just . measCpuTime
                           , secs
                           , "CPU time"))
  , ("utime",              ( fmap fromIntegral . fromInt . measUtime
                           , secs . (/1000000)
                           , "user time"))
  , ("stime",              ( fmap fromIntegral . fromInt . measStime
                           , secs . (/1000000)
                           , "system time"))
  , ("maxrss",             ( fmap fromIntegral . fromInt . measMaxrss
                           , show . rnd
                           , "maximum resident set size"))
  , ("minflt",             ( fmap fromIntegral . fromInt . measMinflt
                           , show . rnd
                           , "minor page faults"))
  , ("majflt",             ( fmap fromIntegral . fromInt . measMajflt
                           , show . rnd
                           , "major page faults"))
  , ("nvcsw",              ( fmap fromIntegral . fromInt . measNvcsw
                           , show . rnd
                           , "voluntary context switches"))
  , ("nivcsw",             ( fmap fromIntegral . fromInt . measNivcsw
                           , show . rnd
                           , "involuntary context switches"))
  , ("allocated",          ( fmap fromIntegral . fromInt . measAllocated
                           , show . rnd
                           , "(+RTS -T) bytes allocated"))
  , ("numGcs",             ( fmap fromIntegral . fromInt . measNumGcs
                           , show . rnd
                           , "(+RTS -T) number of garbage collections"))
  , ("bytesCopied",        ( fmap fromIntegral . fromInt . measBytesCopied
                           , show . rnd
                           , "(+RTS -T) number of bytes copied during GC"))
  , ("mutatorWallSeconds", ( fromDouble . measMutatorWallSeconds
                           , secs
                           , "(+RTS -T) wall-clock time for mutator threads"))
  , ("mutatorCpuSeconds",  ( fromDouble . measMutatorCpuSeconds
                           , secs
                           , "(+RTS -T) CPU time spent running mutator threads"))
  , ("gcWallSeconds",      ( fromDouble . measGcWallSeconds
                           , secs
                           , "(+RTS -T) wall-clock time spent doing GC"))
  , ("gcCpuSeconds",       ( fromDouble . measGcCpuSeconds
                           , secs
                           , "(+RTS -T) CPU time spent doing GC"))
  ]
  where rnd = round :: Double -> Int64

-- | Field names in a 'Measured' record, in the order in which they
-- appear.
measureKeys :: [String]
measureKeys = map fst measureAccessors_

-- | Field names and accessors for a 'Measured' record.
measureAccessors :: Map String ( Measured -> Maybe Double
                               , Double -> String
                               , String
                               )
measureAccessors = fromList measureAccessors_

renderNames :: [String] -> String
renderNames = List.intercalate ", " . map show

-- | Given a list of accessor names (see 'measureKeys'), return either
-- a mapping from accessor name to function or an error message if
-- any names are wrong.
resolveAccessors :: [String]
                 -> Either String [(String, Measured -> Maybe Double)]
resolveAccessors names =
  case unresolved of
    [] -> Right [(n, a) | (n, Just (a,_,_)) <- accessors]
    _  -> Left $ "unknown metric " ++ renderNames unresolved
  where
    unresolved = [n | (n, Nothing) <- accessors]
    accessors = flip map names $ \n -> (n, Map.lookup n measureAccessors)

singleton :: [a] -> Bool
singleton [_] = True
singleton _   = False

-- | Given predictor and responder names, do some basic validation,
-- then hand back the relevant accessors.
validateAccessors :: [String]   -- ^ Predictor names.
                  -> String     -- ^ Responder name.
                  -> Either String [(String, Measured -> Maybe Double)]
validateAccessors predNames respName = do
  when (null predNames) $
    Left "no predictors specified"
  let names = respName:predNames
      dups = map head . filter (not . singleton) .
             List.group . List.sort $ names
  unless (null dups) $
    Left $ "duplicated metric " ++ renderNames dups
  resolveAccessors names

-- | Normalise every measurement as if 'measIters' was 1.
--
-- ('measIters' itself is left unaffected.)
rescale :: Measured -> Measured
rescale m@Measured{..} = m {
    -- skip measIters
      measTime               = d measTime
    , measCycles             = i measCycles
    , measCpuTime            = d measCpuTime

    , measUtime              = i measUtime
    , measStime              = i measStime
    -- skip measMaxrss
    , measMinflt             = i measMinflt
    , measMajflt             = i measMajflt
    , measNvcsw              = i measNvcsw
    , measNivcsw             = i measNivcsw

    , measNumGcs             = i measNumGcs
    , measBytesCopied        = i measBytesCopied
    , measMutatorWallSeconds = d measMutatorWallSeconds
    , measMutatorCpuSeconds  = d measMutatorCpuSeconds
    , measGcWallSeconds      = d measGcWallSeconds
    , measGcCpuSeconds       = d measGcCpuSeconds
    } where
        d k = maybe k (/ iters) (fromDouble k)
        i k = maybe k (round . (/ iters)) (fromIntegral <$> fromInt k)
        iters               = fromIntegral measIters :: Double

-- | Convert a (possibly unavailable) GC measurement to a true value.
-- If the measurement is a huge negative number that corresponds to
-- \"no data\", this will return 'Nothing'.
fromInt :: Int64 -> Maybe Int64
fromInt i | i == minBound = Nothing
          | otherwise     = Just i

-- | Convert from a true value back to the packed representation used
-- for GC measurements.
toInt :: Maybe Int64 -> Int64
toInt Nothing  = minBound
toInt (Just i) = i

-- | Convert a (possibly unavailable) GC measurement to a true value.
-- If the measurement is a huge negative number that corresponds to
-- \"no data\", this will return 'Nothing'.
fromDouble :: Double -> Maybe Double
fromDouble d | isInfinite d || isNaN d = Nothing
             | otherwise               = Just d

-- | Convert from a true value back to the packed representation used
-- for GC measurements.
toDouble :: Maybe Double -> Double
toDouble Nothing  = -1/0
toDouble (Just d) = d
