{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, GADTs, RecordWildCards #-}
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
    -- * Benchmark descriptions
    , Benchmarkable(..)
    , Benchmark(..)
    -- * Measurements
    , Measured(..)
    , fromInt
    , toInt
    , fromDouble
    , toDouble
    , measureAccessors
    , measureKeys
    , rescale
    -- * Benchmark construction
    , env
    , envWithCleanup
    , perBatchEnv
    , perBatchEnvWithCleanup
    , perRunEnv
    , perRunEnvWithCleanup
    , toBenchmarkable
    , bench
    , bgroup
    , addPrefix
    , benchNames
    -- ** Evaluation control
    , whnf
    , nf
    , nfIO
    , whnfIO
    -- * Result types
    , Outliers(..)
    , OutlierEffect(..)
    , OutlierVariance(..)
    , Regression(..)
    , KDE(..)
    , Report(..)
    , SampleAnalysis(..)
    ) where

-- Temporary: to support pre-AMP GHC 7.8.4:
import Control.Applicative
import Data.Monoid

import Control.DeepSeq (NFData(rnf))
import Control.Exception (evaluate)
import Data.Data (Data, Typeable)
import Data.Int (Int64)
import Data.Map (Map, fromList)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Statistics.Types as St
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
      confInterval :: St.CL Double
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


-- | A pure function or impure action that can be benchmarked. The
-- 'Int64' parameter indicates the number of times to run the given
-- function or action.
data Benchmarkable = forall a . NFData a =>
    Benchmarkable
      { allocEnv :: Int64 -> IO a
      , cleanEnv :: Int64 -> a -> IO ()
      , runRepeatedly :: a -> Int64 -> IO ()
      , perRun :: Bool
      }

noop :: Monad m => a -> m ()
noop = const $ return ()
{-# INLINE noop #-}

-- | Construct a 'Benchmarkable' value from an impure action, where the 'Int64'
-- parameter indicates the number of times to run the action.
toBenchmarkable :: (Int64 -> IO ()) -> Benchmarkable
toBenchmarkable f = Benchmarkable noop (const noop) (const f) False
{-# INLINE toBenchmarkable #-}

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
      measTime               :: !Double
      -- ^ Total wall-clock time elapsed, in seconds.
    , measCpuTime            :: !Double
      -- ^ Total CPU time elapsed, in seconds.  Includes both user and
      -- kernel (system) time.
    , measCycles             :: !Int64
      -- ^ Cycles, in unspecified units that may be CPU cycles.  (On
      -- i386 and x86_64, this is measured using the @rdtsc@
      -- instruction.)
    , measIters              :: !Int64
      -- ^ Number of loop iterations measured.

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

-- THIS MUST REFLECT THE ORDER OF FIELDS IN THE DATA TYPE.
--
-- The ordering is used by Javascript code to pick out the correct
-- index into the vector that represents a Measured value in that
-- world.
measureAccessors_ :: [(String, (Measured -> Maybe Double, String))]
measureAccessors_ = [
    ("time",               (Just . measTime,
                            "wall-clock time"))
  , ("cpuTime",            (Just . measCpuTime,
                            "CPU time"))
  , ("cycles",             (Just . fromIntegral . measCycles,
                            "CPU cycles"))
  , ("iters",              (Just . fromIntegral . measIters,
                            "loop iterations"))
  , ("allocated",          (fmap fromIntegral . fromInt . measAllocated,
                            "(+RTS -T) bytes allocated"))
  , ("numGcs",             (fmap fromIntegral . fromInt . measNumGcs,
                            "(+RTS -T) number of garbage collections"))
  , ("bytesCopied",        (fmap fromIntegral . fromInt . measBytesCopied,
                            "(+RTS -T) number of bytes copied during GC"))
  , ("mutatorWallSeconds", (fromDouble . measMutatorWallSeconds,
                            "(+RTS -T) wall-clock time for mutator threads"))
  , ("mutatorCpuSeconds",  (fromDouble . measMutatorCpuSeconds,
                            "(+RTS -T) CPU time spent running mutator threads"))
  , ("gcWallSeconds",      (fromDouble . measGcWallSeconds,
                            "(+RTS -T) wall-clock time spent doing GC"))
  , ("gcCpuSeconds",       (fromDouble . measGcCpuSeconds,
                            "(+RTS -T) CPU time spent doing GC"))
  ]

-- | Field names in a 'Measured' record, in the order in which they
-- appear.
measureKeys :: [String]
measureKeys = map fst measureAccessors_

-- | Field names and accessors for a 'Measured' record.
measureAccessors :: Map String (Measured -> Maybe Double, String)
measureAccessors = fromList measureAccessors_

-- | Normalise every measurement as if 'measIters' was 1.
--
-- ('measIters' itself is left unaffected.)
rescale :: Measured -> Measured
rescale m@Measured{..} = m {
      measTime               = d measTime
    , measCpuTime            = d measCpuTime
    , measCycles             = i measCycles
    -- skip measIters
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

-- | Apply an argument to a function, and evaluate the result to weak
-- head normal form (WHNF).
whnf :: (a -> b) -> a -> Benchmarkable
whnf = pureFunc id
{-# INLINE whnf #-}

-- | Apply an argument to a function, and evaluate the result to
-- normal form (NF).
nf :: NFData b => (a -> b) -> a -> Benchmarkable
nf = pureFunc rnf
{-# INLINE nf #-}

pureFunc :: (b -> c) -> (a -> b) -> a -> Benchmarkable
pureFunc reduce f0 x0 = toBenchmarkable (go f0 x0)
  where go f x n
          | n <= 0    = return ()
          | otherwise = evaluate (reduce (f x)) >> go f x (n-1)
{-# INLINE pureFunc #-}

-- | Perform an action, then evaluate its result to normal form.
-- This is particularly useful for forcing a lazy 'IO' action to be
-- completely performed.
nfIO :: NFData a => IO a -> Benchmarkable
nfIO = toBenchmarkable . impure rnf
{-# INLINE nfIO #-}

-- | Perform an action, then evaluate its result to weak head normal
-- form (WHNF).  This is useful for forcing an 'IO' action whose result
-- is an expression to be evaluated down to a more useful value.
whnfIO :: IO a -> Benchmarkable
whnfIO = toBenchmarkable . impure id
{-# INLINE whnfIO #-}

impure :: (a -> b) -> IO a -> Int64 -> IO ()
impure strategy a = go
  where go n
          | n <= 0    = return ()
          | otherwise = a >>= (evaluate . strategy) >> go (n-1)
{-# INLINE impure #-}

-- | Specification of a collection of benchmarks and environments. A
-- benchmark may consist of:
--
-- * An environment that creates input data for benchmarks, created
--   with 'env'.
--
-- * A single 'Benchmarkable' item with a name, created with 'bench'.
--
-- * A (possibly nested) group of 'Benchmark's, created with 'bgroup'.
data Benchmark where
    Environment  :: NFData env
                 => IO env -> (env -> IO a) -> (env -> Benchmark) -> Benchmark
    Benchmark    :: String -> Benchmarkable -> Benchmark
    BenchGroup   :: String -> [Benchmark] -> Benchmark

-- | Run a benchmark (or collection of benchmarks) in the given
-- environment.  The purpose of an environment is to lazily create
-- input data to pass to the functions that will be benchmarked.
--
-- A common example of environment data is input that is read from a
-- file.  Another is a large data structure constructed in-place.
--
-- By deferring the creation of an environment when its associated
-- benchmarks need the its, we avoid two problems that this strategy
-- caused:
--
-- * Memory pressure distorted the results of unrelated benchmarks.
--   If one benchmark needed e.g. a gigabyte-sized input, it would
--   force the garbage collector to do extra work when running some
--   other benchmark that had no use for that input.  Since the data
--   created by an environment is only available when it is in scope,
--   it should be garbage collected before other benchmarks are run.
--
-- * The time cost of generating all needed inputs could be
--   significant in cases where no inputs (or just a few) were really
--   needed.  This occurred often, for instance when just one out of a
--   large suite of benchmarks was run, or when a user would list the
--   collection of benchmarks without running any.
--
-- __Creation.__ An environment is created right before its related
-- benchmarks are run.  The 'IO' action that creates the environment
-- is run, then the newly created environment is evaluated to normal
-- form (hence the 'NFData' constraint) before being passed to the
-- function that receives the environment.
--
-- __Complex environments.__ If you need to create an environment that
-- contains multiple values, simply pack the values into a tuple.
--
-- __Lazy pattern matching.__ In situations where a \"real\"
-- environment is not needed, e.g. if a list of benchmark names is
-- being generated, @undefined@ will be passed to the function that
-- receives the environment.  This avoids the overhead of generating
-- an environment that will not actually be used.
--
-- The function that receives the environment must use lazy pattern
-- matching to deconstruct the tuple, as use of strict pattern
-- matching will cause a crash if @undefined@ is passed in.
--
-- __Example.__ This program runs benchmarks in an environment that
-- contains two values.  The first value is the contents of a text
-- file; the second is a string.  Pay attention to the use of a lazy
-- pattern to deconstruct the tuple in the function that returns the
-- benchmarks to be run.
--
-- > setupEnv = do
-- >   let small = replicate 1000 (1 :: Int)
-- >   big <- map length . words <$> readFile "/usr/dict/words"
-- >   return (small, big)
-- >
-- > main = defaultMain [
-- >    -- notice the lazy pattern match here!
-- >    env setupEnv $ \ ~(small,big) -> bgroup "main" [
-- >    bgroup "small" [
-- >      bench "length" $ whnf length small
-- >    , bench "length . filter" $ whnf (length . filter (==1)) small
-- >    ]
-- >  ,  bgroup "big" [
-- >      bench "length" $ whnf length big
-- >    , bench "length . filter" $ whnf (length . filter (==1)) big
-- >    ]
-- >  ] ]
--
-- __Discussion.__ The environment created in the example above is
-- intentionally /not/ ideal.  As Haskell's scoping rules suggest, the
-- variable @big@ is in scope for the benchmarks that use only
-- @small@.  It would be better to create a separate environment for
-- @big@, so that it will not be kept alive while the unrelated
-- benchmarks are being run.
env :: NFData env =>
       IO env
    -- ^ Create the environment.  The environment will be evaluated to
    -- normal form before being passed to the benchmark.
    -> (env -> Benchmark)
    -- ^ Take the newly created environment and make it available to
    -- the given benchmarks.
    -> Benchmark
env alloc = Environment alloc noop

-- | Same as `env`, but but allows for an additional callback
-- to clean up the environment. Resource clean up is exception safe, that is,
-- it runs even if the 'Benchmark' throws an exception.
envWithCleanup
    :: NFData env
    => IO env
    -- ^ Create the environment.  The environment will be evaluated to
    -- normal form before being passed to the benchmark.
    -> (env -> IO a)
    -- ^ Clean up the created environment.
    -> (env -> Benchmark)
    -- ^ Take the newly created environment and make it available to
    -- the given benchmarks.
    -> Benchmark
envWithCleanup = Environment

-- | Create a Benchmarkable where a fresh environment is allocated for every
-- batch of runs of the benchmarkable.
--
-- The environment is evaluated to normal form before the benchmark is run.
--
-- When using 'whnf', 'whnfIO', etc. Gauge creates a 'Benchmarkable'
-- whichs runs a batch of @N@ repeat runs of that expressions. Gauge may
-- run any number of these batches to get accurate measurements. Environments
-- created by 'env' and 'envWithCleanup', are shared across all these batches
-- of runs.
--
-- This is fine for simple benchmarks on static input, but when benchmarking
-- IO operations where these operations can modify (and especially grow) the
-- environment this means that later batches might have their accuracy effected
-- due to longer, for example, longer garbage collection pauses.
--
-- An example: Suppose we want to benchmark writing to a Chan, if we allocate
-- the Chan using environment and our benchmark consists of @writeChan env ()@,
-- the contents and thus size of the Chan will grow with every repeat. If
-- Gauge runs a 1,000 batches of 1,000 repeats, the result is that the
-- channel will have 999,000 items in it by the time the last batch is run.
-- Since GHC GC has to copy the live set for every major GC this means our last
-- set of writes will suffer a lot of noise of the previous repeats.
--
-- By allocating a fresh environment for every batch of runs this function
-- should eliminate this effect.
perBatchEnv
    :: (NFData env, NFData b)
    => (Int64 -> IO env)
    -- ^ Create an environment for a batch of N runs. The environment will be
    -- evaluated to normal form before running.
    -> (env -> IO b)
    -- ^ Function returning the IO action that should be benchmarked with the
    -- newly generated environment.
    -> Benchmarkable
perBatchEnv alloc = perBatchEnvWithCleanup alloc (const noop)

-- | Same as `perBatchEnv`, but but allows for an additional callback
-- to clean up the environment. Resource clean up is exception safe, that is,
-- it runs even if the 'Benchmark' throws an exception.
perBatchEnvWithCleanup
    :: (NFData env, NFData b)
    => (Int64 -> IO env)
    -- ^ Create an environment for a batch of N runs. The environment will be
    -- evaluated to normal form before running.
    -> (Int64 -> env -> IO ())
    -- ^ Clean up the created environment.
    -> (env -> IO b)
    -- ^ Function returning the IO action that should be benchmarked with the
    -- newly generated environment.
    -> Benchmarkable
perBatchEnvWithCleanup alloc clean work
    = Benchmarkable alloc clean (impure rnf . work) False

-- | Create a Benchmarkable where a fresh environment is allocated for every
-- run of the operation to benchmark. This is useful for benchmarking mutable
-- operations that need a fresh environment, such as sorting a mutable Vector.
--
-- As with 'env' and 'perBatchEnv' the environment is evaluated to normal form
-- before the benchmark is run.
--
-- This introduces extra noise and result in reduce accuracy compared to other
-- Gauge benchmarks. But allows easier benchmarking for mutable operations
-- than was previously possible.
perRunEnv
    :: (NFData env, NFData b)
    => IO env
    -- ^ Action that creates the environment for a single run.
    -> (env -> IO b)
    -- ^ Function returning the IO action that should be benchmarked with the
    -- newly genereted environment.
    -> Benchmarkable
perRunEnv alloc = perRunEnvWithCleanup alloc noop

-- | Same as `perRunEnv`, but but allows for an additional callback
-- to clean up the environment. Resource clean up is exception safe, that is,
-- it runs even if the 'Benchmark' throws an exception.
perRunEnvWithCleanup
    :: (NFData env, NFData b)
    => IO env
    -- ^ Action that creates the environment for a single run.
    -> (env -> IO ())
    -- ^ Clean up the created environment.
    -> (env -> IO b)
    -- ^ Function returning the IO action that should be benchmarked with the
    -- newly genereted environment.
    -> Benchmarkable
perRunEnvWithCleanup alloc clean work = bm { perRun = True }
  where
    bm = perBatchEnvWithCleanup (const alloc) (const clean) work

-- | Create a single benchmark.
bench :: String                 -- ^ A name to identify the benchmark.
      -> Benchmarkable          -- ^ An activity to be benchmarked.
      -> Benchmark
bench = Benchmark

-- | Group several benchmarks together under a common name.
bgroup :: String                -- ^ A name to identify the group of benchmarks.
       -> [Benchmark]           -- ^ Benchmarks to group under this name.
       -> Benchmark
bgroup = BenchGroup

-- | Add the given prefix to a name.  If the prefix is empty, the name
-- is returned unmodified.  Otherwise, the prefix and name are
-- separated by a @\'\/\'@ character.
addPrefix :: String             -- ^ Prefix.
          -> String             -- ^ Name.
          -> String
addPrefix ""  desc = desc
addPrefix pfx desc = pfx ++ '/' : desc

-- | Retrieve the names of all benchmarks.  Grouped benchmarks are
-- prefixed with the name of the group they're in.
benchNames :: Benchmark -> [String]
benchNames (Environment _ _ b) = benchNames (b undefined)
benchNames (Benchmark d _)   = [d]
benchNames (BenchGroup d bs) = map (addPrefix d) . concatMap benchNames $ bs

instance Show Benchmark where
    show (Environment _ _ b) = "Environment _ _" ++ show (b undefined)
    show (Benchmark d _)   = "Benchmark " ++ show d
    show (BenchGroup d _)  = "BenchGroup " ++ show d

-- | Outliers from sample data, calculated using the boxplot
-- technique.
data Outliers = Outliers {
      samplesSeen :: !Int64
    , lowSevere   :: !Int64
    -- ^ More than 3 times the interquartile range (IQR) below the
    -- first quartile.
    , lowMild     :: !Int64
    -- ^ Between 1.5 and 3 times the IQR below the first quartile.
    , highMild    :: !Int64
    -- ^ Between 1.5 and 3 times the IQR above the third quartile.
    , highSevere  :: !Int64
    -- ^ More than 3 times the IQR above the third quartile.
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance NFData Outliers

-- | A description of the extent to which outliers in the sample data
-- affect the sample mean and standard deviation.
data OutlierEffect = Unaffected -- ^ Less than 1% effect.
                   | Slight     -- ^ Between 1% and 10%.
                   | Moderate   -- ^ Between 10% and 50%.
                   | Severe     -- ^ Above 50% (i.e. measurements
                                -- are useless).
                     deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

instance NFData OutlierEffect

instance Monoid Outliers where
    mempty  = Outliers 0 0 0 0 0
    mappend = addOutliers

addOutliers :: Outliers -> Outliers -> Outliers
addOutliers (Outliers s a b c d) (Outliers t w x y z) =
    Outliers (s+t) (a+w) (b+x) (c+y) (d+z)
{-# INLINE addOutliers #-}

-- | Analysis of the extent to which outliers in a sample affect its
-- standard deviation (and to some extent, its mean).
data OutlierVariance = OutlierVariance {
      ovEffect   :: OutlierEffect
    -- ^ Qualitative description of effect.
    , ovDesc     :: String
    -- ^ Brief textual description of effect.
    , ovFraction :: Double
    -- ^ Quantitative description of effect (a fraction between 0 and 1).
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance NFData OutlierVariance where
    rnf OutlierVariance{..} = rnf ovEffect `seq` rnf ovDesc `seq` rnf ovFraction

-- | Results of a linear regression.
data Regression = Regression {
    regResponder  :: String
    -- ^ Name of the responding variable.
  , regCoeffs     :: Map String (St.Estimate St.ConfInt Double)
    -- ^ Map from name to value of predictor coefficients.
  , regRSquare    :: St.Estimate St.ConfInt Double
    -- ^ R&#0178; goodness-of-fit estimate.
  } deriving (Eq, Read, Show, Typeable, Generic)

instance NFData Regression where
    rnf Regression{..} =
      rnf regResponder `seq` rnf regCoeffs `seq` rnf regRSquare

-- | Result of a bootstrap analysis of a non-parametric sample.
data SampleAnalysis = SampleAnalysis {
      anRegress    :: [Regression]
      -- ^ Estimates calculated via linear regression.
    , anOverhead   :: Double
      -- ^ Estimated measurement overhead, in seconds.  Estimation is
      -- performed via linear regression.
    , anMean       :: St.Estimate St.ConfInt Double
      -- ^ Estimated mean.
    , anStdDev     :: St.Estimate St.ConfInt Double
      -- ^ Estimated standard deviation.
    , anOutlierVar :: OutlierVariance
      -- ^ Description of the effects of outliers on the estimated
      -- variance.
    } deriving (Eq, Read, Show, Typeable, Generic)

instance NFData SampleAnalysis where
    rnf SampleAnalysis{..} =
        rnf anRegress `seq` rnf anOverhead `seq` rnf anMean `seq`
        rnf anStdDev `seq` rnf anOutlierVar

-- | Data for a KDE chart of performance.
data KDE = KDE {
      kdeType   :: String
    , kdeValues :: U.Vector Double
    , kdePDF    :: U.Vector Double
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

instance NFData KDE where
    rnf KDE{..} = rnf kdeType `seq` rnf kdeValues `seq` rnf kdePDF

-- | Report of a sample analysis.
data Report = Report {
      reportName     :: String
      -- ^ The name of this report.
    , reportKeys     :: [String]
      -- ^ See 'measureKeys'.
    , reportMeasured :: V.Vector Measured
      -- ^ Raw measurements. These are /not/ corrected for the
      -- estimated measurement overhead that can be found via the
      -- 'anOverhead' field of 'reportAnalysis'.
    , reportAnalysis :: SampleAnalysis
      -- ^ Report analysis.
    , reportOutliers :: Outliers
      -- ^ Analysis of outliers.
    , reportKDEs     :: [KDE]
      -- ^ Data for a KDE of times.
    } deriving (Eq, Read, Show, Typeable, Generic)

instance NFData Report where
    rnf Report{..} =
      rnf reportName `seq` rnf reportKeys `seq`
      rnf reportMeasured `seq` rnf reportAnalysis `seq` rnf reportOutliers `seq`
      rnf reportKDEs
