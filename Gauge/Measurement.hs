{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns, CPP, ForeignFunctionInterface,
    ScopedTypeVariables #-}

#if MIN_VERSION_base(4,10,0)
-- Disable deprecation warnings for now until we remove the use of getGCStats
-- and applyGCStats for good
{-# OPTIONS_GHC -Wno-deprecations #-}
#endif
#ifdef mingw32_HOST_OS
-- Disable warning about RUsage being unused on Windows
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
#endif

-- |
-- Module      : Gauge.Measurement
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Benchmark measurement code.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Gauge.Measurement
    (
      initializeTime
    , getTime
    , getCPUTime
    , getCycles
    , getGCStatistics
    , GCStatistics(..)
    , ClockTime(..)
    , CpuTime(..)
    , Cycles(..)
    , MeasureDiff(..)
    , measure
    , runBenchmark
    , runBenchmarkable
    , runBenchmarkable_
    , measured
    , applyGCStatistics
      -- * Deprecated
    , getGCStats
    , applyGCStats
    ) where

import Gauge.Types (Benchmarkable(..), Measured(..))
import Control.Applicative ((<*))
import Control.DeepSeq (NFData(rnf))
import Control.Exception (finally,evaluate)
import Data.Data (Data, Typeable)
import Data.Int (Int64)
import Data.List (unfoldr)
import Data.Word (Word64, Word8)
#ifndef mingw32_HOST_OS
import Foreign.C (CLong(..))
import Foreign.Ptr (Ptr)
#endif
import GHC.Generics (Generic)
import GHC.Stats (GCStats(..))
#if MIN_VERSION_base(4,10,0)
import GHC.Stats (RTSStats(..), GCDetails(..))
#endif
import System.Mem (performGC)
import Foreign.Ptr
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable
import qualified Control.Exception as Exc
import qualified Data.Vector as V
import qualified GHC.Stats as Stats

#define GAUGE_MEASURE_TIME_NEW

data MeasurementType = Differential | Absolute

newtype ClockTime (ty :: MeasurementType) = ClockTime Word64
    deriving (Eq)
newtype CpuTime (ty :: MeasurementType) = CpuTime Word64
    deriving (Eq)
newtype Cycles (ty :: MeasurementType) = Cycles Word64
    deriving (Eq)

data TimeRecord w = TimeRecord
    {-# UNPACK #-} !(ClockTime w)
    {-# UNPACK #-} !(CpuTime w)
    {-# UNPACK #-} !(Cycles w)

class MeasureDiff w where
    measureDiff :: w 'Absolute -> w 'Absolute -> w 'Differential

instance MeasureDiff ClockTime where
    measureDiff (ClockTime end) (ClockTime start)
        | end > start = ClockTime d
        | otherwise   = ClockTime 0
      where d = end - start
instance MeasureDiff CpuTime where
    measureDiff (CpuTime end) (CpuTime start)
        | end > start = CpuTime d
        | otherwise   = CpuTime 0
      where d = end - start
instance MeasureDiff Cycles where
    measureDiff (Cycles end) (Cycles start)
        | end > start = Cycles d
        | otherwise   = Cycles 0
      where d = end - start
instance MeasureDiff TimeRecord where
    measureDiff (TimeRecord a1 b1 c1) (TimeRecord a2 b2 c2) =
        TimeRecord (measureDiff a1 a2)
                   (measureDiff b1 b2)
                   (measureDiff c1 c2)

-- | Statistics about memory usage and the garbage collector. Apart from
-- 'gcStatsCurrentBytesUsed' and 'gcStatsCurrentBytesSlop' all are cumulative values since
-- the program started.
--
-- 'GCStatistics' is cargo-culted from the 'GCStats' data type that "GHC.Stats"
-- has. Since 'GCStats' was marked as deprecated and will be removed in GHC 8.4,
-- we use 'GCStatistics' to provide a backwards-compatible view of GC statistics.
data GCStatistics = GCStatistics
    { -- | Total number of bytes allocated
    gcStatsBytesAllocated :: !Int64
    -- | Number of garbage collections performed (any generation, major and
    -- minor)
    , gcStatsNumGcs :: !Int64
    -- | Maximum number of live bytes seen so far
    , gcStatsMaxBytesUsed :: !Int64
    -- | Number of byte usage samples taken, or equivalently
    -- the number of major GCs performed.
    , gcStatsNumByteUsageSamples :: !Int64
    -- | Sum of all byte usage samples, can be used with
    -- 'gcStatsNumByteUsageSamples' to calculate averages with
    -- arbitrary weighting (if you are sampling this record multiple
    -- times).
    , gcStatsCumulativeBytesUsed :: !Int64
    -- | Number of bytes copied during GC
    , gcStatsBytesCopied :: !Int64
    -- | Number of live bytes at the end of the last major GC
    , gcStatsCurrentBytesUsed :: !Int64
    -- | Current number of bytes lost to slop
    , gcStatsCurrentBytesSlop :: !Int64
    -- | Maximum number of bytes lost to slop at any one time so far
    , gcStatsMaxBytesSlop :: !Int64
    -- | Maximum number of megabytes allocated
    , gcStatsPeakMegabytesAllocated :: !Int64
    -- | CPU time spent running mutator threads.  This does not include
    -- any profiling overhead or initialization.
    , gcStatsMutatorCpuSeconds :: !Double

    -- | Wall clock time spent running mutator threads.  This does not
    -- include initialization.
    , gcStatsMutatorWallSeconds :: !Double
    -- | CPU time spent running GC
    , gcStatsGcCpuSeconds :: !Double
    -- | Wall clock time spent running GC
    , gcStatsGcWallSeconds :: !Double
    -- | Total CPU time elapsed since program start
    , gcStatsCpuSeconds :: !Double
    -- | Total wall clock time elapsed since start
    , gcStatsWallSeconds :: !Double
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

-- | Try to get GC statistics, bearing in mind that the GHC runtime
-- will throw an exception if statistics collection was not enabled
-- using \"@+RTS -T@\".
{-# DEPRECATED getGCStats
      ["GCStats has been deprecated in GHC 8.2. As a consequence,",
       "getGCStats has also been deprecated in favor of getGCStatistics.",
       "getGCStats will be removed in the next major gauge release."] #-}
getGCStats :: IO (Maybe GCStats)
getGCStats =
  (Just `fmap` Stats.getGCStats) `Exc.catch` \(_::Exc.SomeException) ->
  return Nothing

-- | Try to get GC statistics, bearing in mind that the GHC runtime
-- will throw an exception if statistics collection was not enabled
-- using \"@+RTS -T@\".
getGCStatistics :: IO (Maybe GCStatistics)
#if MIN_VERSION_base(4,10,0)
-- Use RTSStats/GCDetails to gather GC stats
getGCStatistics = do
  hasStats <- Stats.getRTSStatsEnabled
  if hasStats
  then getStats >>= return . Just
  else return Nothing

  where

  getStats = do
    stats <- Stats.getRTSStats
    let gcdetails :: Stats.GCDetails
        gcdetails = gc stats

        nsToSecs :: Int64 -> Double
        nsToSecs ns = fromIntegral ns * 1.0E-9

    return $ GCStatistics {
        gcStatsBytesAllocated         = fromIntegral $ allocated_bytes stats
      , gcStatsNumGcs                 = fromIntegral $ gcs stats
      , gcStatsMaxBytesUsed           = fromIntegral $ max_live_bytes stats
      , gcStatsNumByteUsageSamples    = fromIntegral $ major_gcs stats
      , gcStatsCumulativeBytesUsed    = fromIntegral $ cumulative_live_bytes stats
      , gcStatsBytesCopied            = fromIntegral $ copied_bytes stats
      , gcStatsCurrentBytesUsed       = fromIntegral $ gcdetails_live_bytes gcdetails
      , gcStatsCurrentBytesSlop       = fromIntegral $ gcdetails_slop_bytes gcdetails
      , gcStatsMaxBytesSlop           = fromIntegral $ max_slop_bytes stats
      , gcStatsPeakMegabytesAllocated = fromIntegral (max_mem_in_use_bytes stats) `quot` (1024*1024)
      , gcStatsMutatorCpuSeconds      = nsToSecs $ mutator_cpu_ns stats
      , gcStatsMutatorWallSeconds     = nsToSecs $ mutator_elapsed_ns stats
      , gcStatsGcCpuSeconds           = nsToSecs $ gc_cpu_ns stats
      , gcStatsGcWallSeconds          = nsToSecs $ gc_elapsed_ns stats
      , gcStatsCpuSeconds             = nsToSecs $ cpu_ns stats
      , gcStatsWallSeconds            = nsToSecs $ elapsed_ns stats
      }
#else
-- Use the old GCStats type to gather GC stats
getGCStatistics = do
  stats <- Stats.getGCStats
  return $ Just GCStatistics {
      gcStatsBytesAllocated         = bytesAllocated stats
    , gcStatsNumGcs                 = numGcs stats
    , gcStatsMaxBytesUsed           = maxBytesUsed stats
    , gcStatsNumByteUsageSamples    = numByteUsageSamples stats
    , gcStatsCumulativeBytesUsed    = cumulativeBytesUsed stats
    , gcStatsBytesCopied            = bytesCopied stats
    , gcStatsCurrentBytesUsed       = currentBytesUsed stats
    , gcStatsCurrentBytesSlop       = currentBytesSlop stats
    , gcStatsMaxBytesSlop           = maxBytesSlop stats
    , gcStatsPeakMegabytesAllocated = peakMegabytesAllocated stats
    , gcStatsMutatorCpuSeconds      = mutatorCpuSeconds stats
    , gcStatsMutatorWallSeconds     = mutatorWallSeconds stats
    , gcStatsGcCpuSeconds           = gcCpuSeconds stats
    , gcStatsGcWallSeconds          = gcWallSeconds stats
    , gcStatsCpuSeconds             = cpuSeconds stats
    , gcStatsWallSeconds            = wallSeconds stats
    }
 `Exc.catch`
  \(_::Exc.SomeException) -> return Nothing
#endif

data RUsage = RUsage
    { ruUtime  :: Int64
    , ruStime  :: Int64
    , ruMaxrss :: Int64
    , ruMinflt :: Int64
    , ruMajflt :: Int64
    , ruNvcsw  :: Int64
    , ruNivcsw :: Int64
    }

getRUsage :: IO (Maybe RUsage)
getRUsage = do
#ifdef mingw32_HOST_OS
    return Nothing
#else
    ru <- c_getRUsage
    -- ru points to static memory, copy it over
    utime  <- c_getRUutime ru
    stime  <- c_getRUstime ru
    maxrss <- c_getRUmaxrss ru
    minflt <- c_getRUminflt ru
    majflt <- c_getRUmajflt ru
    nvcsw  <- c_getRUnvcsw ru
    nivcsw <- c_getRUnivcsw ru

    return $ Just $ RUsage
        { ruUtime  = (fromInteger . toInteger) utime
        , ruStime  = (fromInteger . toInteger) stime
        , ruMaxrss = (fromInteger . toInteger) maxrss
        , ruMinflt = (fromInteger . toInteger) minflt
        , ruMajflt = (fromInteger . toInteger) majflt
        , ruNvcsw  = (fromInteger . toInteger) nvcsw
        , ruNivcsw = (fromInteger . toInteger) nivcsw
        }
#endif

#ifdef GAUGE_MEASURE_TIME_NEW
measureTime :: IO () -> IO (TimeRecord 'Differential)
measureTime f = allocaBytes 64 $ \ptr -> do
    getRecordPtr ptr
    f
    getRecordPtr (ptr `plusPtr` 32)
    start <- ptrToTimeRecord ptr
    end   <- ptrToTimeRecord (ptr `plusPtr` 32)
    pure $! measureDiff end start
#else
measureTime :: IO () -> IO (Double, Double, Word64)
measureTime f = do
    startTime  <- getTime
    startCpu   <- getCPUTime
    startCycle <- getCycles
    f
    endTime  <- getTime
    endCpu   <- getCPUTime
    endCycle <- getCycles
    pure ( max 0 (endTime - startTime)
         , max 0 (endCpu - startCpu)
         , max 0 (endCycle - startCycle))
#endif
{-# INLINE measureTime #-}

-- | Measure the execution of a benchmark a given number of times.
measure :: Benchmarkable -- ^ Operation to benchmark.
        -> Int64         -- ^ Number of iterations.
        -> IO Measured
measure bm iters = runBenchmarkable bm iters addResults $ \act -> do
  startStats <- getGCStatistics
  startRUsage <- getRUsage
#ifdef GAUGE_MEASURE_TIME_NEW
  TimeRecord time cpuTime cycles <- measureTime act
#else
  (time, cpuTime, cycles) <- measureTime act
#endif
  endRUsage <- getRUsage
  endStats <- getGCStatistics
  let !m = applyGCStatistics endStats startStats $
           applyRUStatistics endRUsage startRUsage $ measured {
             measTime    = outTime time
           , measCpuTime = outCputime cpuTime
           , measCycles  = outCycles cycles
           , measIters   = iters
           }
  return m
  where
#ifdef GAUGE_MEASURE_TIME_NEW
    outTime (ClockTime w)  = fromIntegral w / 1.0e9
    outCputime (CpuTime w) = fromIntegral w / 1.0e9
    outCycles (Cycles w)   = fromIntegral w
#else
    outTime w = w
    outCputime w = w
    outCycles w = fromIntegral w
#endif
    addResults :: Measured -> Measured -> Measured
    addResults !m1 !m2 = m3
      where
        add f = f m1 + f m2

        m3 = Measured
            { measTime               = add measTime
            , measCpuTime            = add measCpuTime
            , measCycles             = add measCycles
            , measIters              = add measIters

            , measUtime              = add measUtime
            , measStime              = add measStime
            , measMaxrss             = max (measMaxrss m1) (measMaxrss m2)
            , measMinflt             = add measMinflt
            , measMajflt             = add measMajflt
            , measNvcsw              = add measNvcsw
            , measNivcsw             = add measNivcsw

            , measAllocated          = add measAllocated
            , measNumGcs             = add measNumGcs
            , measBytesCopied        = add measBytesCopied
            , measMutatorWallSeconds = add measMutatorWallSeconds
            , measMutatorCpuSeconds  = add measMutatorCpuSeconds
            , measGcWallSeconds      = add measGcWallSeconds
            , measGcCpuSeconds       = add measGcCpuSeconds
            }
{-# INLINE measure #-}

runBenchmarkable :: Benchmarkable
                 -> Int64
                 -> (a -> a -> a)
                 -> (IO () -> IO a)
                 -> IO a
runBenchmarkable Benchmarkable{..} i comb f
    | perRun = work >>= go (i - 1)
    | otherwise = work
  where
    go 0 result = return result
    go !n !result = work >>= go (n - 1) . comb result

    count | perRun = 1
          | otherwise = i

    work = do
        env <- allocEnv count
        let clean = cleanEnv count env
            run = runRepeatedly env count

        clean `seq` run `seq` evaluate $ rnf env

        performGC
        f run `finally` clean <* performGC
    {-# INLINE work #-}
{-# INLINE runBenchmarkable #-}

runBenchmarkable_ :: Benchmarkable -> Int64 -> IO ()
runBenchmarkable_ bm i = runBenchmarkable bm i (\() () -> ()) id
{-# INLINE runBenchmarkable_ #-}

-- | Run a single benchmark, and return measurements collected while executing
-- it, along with the amount of time the measurement process took. The
-- benchmark will not terminate until we reach all the minimum bounds
-- specified. If the minimum bounds are satisfied, the benchmark will terminate
-- as soon as we reach any of the maximums.
runBenchmark :: Benchmarkable
             -> Int
             -- ^ Minimum sample duration to in ms.
             -> Int
             -- ^ Minimum number of samples.
             -> Double
             -- ^ Upper bound on how long the benchmarking process
             -- should take.
             -> IO (V.Vector Measured, Double)
runBenchmark bm minDuration minSamples timeLimit = do
  runBenchmarkable_ bm 1
  start <- performGC >> getTime
  let loop [] !_ _ = error "unpossible!"
      loop (iters:niters) samples acc = do
        m <- measure bm iters
        endTime <- getTime
        if samples >= minSamples &&
           measTime m >= fromIntegral minDuration / 1000 &&
           endTime - start >= timeLimit
          then do
            let !v = V.reverse (V.fromList acc)
            return (v, endTime - start)
          else if measTime m >= fromIntegral minDuration / 1000
               then loop niters (samples + 1) (m:acc)
               else loop niters samples (acc)
  loop (squish (unfoldr series 1)) 0 []

-- Our series starts its growth very slowly when we begin at 1, so we
-- eliminate repeated values.
squish :: (Eq a) => [a] -> [a]
squish ys = foldr go [] ys
  where go x xs = x : dropWhile (==x) xs

series :: Double -> Maybe (Int64, Double)
series k = Just (truncate l, l)
  where l = k * 1.05

-- | An empty structure.
measured :: Measured
measured = Measured {
      measTime               = 0
    , measCpuTime            = 0
    , measCycles             = 0
    , measIters              = 0

    , measUtime              = minBound
    , measStime              = minBound
    , measMaxrss             = minBound
    , measMinflt             = minBound
    , measMajflt             = minBound
    , measNvcsw              = minBound
    , measNivcsw             = minBound

    , measAllocated          = minBound
    , measNumGcs             = minBound
    , measBytesCopied        = minBound
    , measMutatorWallSeconds = bad
    , measMutatorCpuSeconds  = bad
    , measGcWallSeconds      = bad
    , measGcCpuSeconds       = bad
    } where bad = -1/0

-- | Apply the difference between two sets of GC statistics to a
-- measurement.
{-# DEPRECATED applyGCStats
      ["GCStats has been deprecated in GHC 8.2. As a consequence,",
       "applyGCStats has also been deprecated in favor of applyGCStatistics.",
       "applyGCStats will be removed in the next major gauge release."] #-}
applyGCStats :: Maybe GCStats
             -- ^ Statistics gathered at the __end__ of a run.
             -> Maybe GCStats
             -- ^ Statistics gathered at the __beginning__ of a run.
             -> Measured
             -- ^ Value to \"modify\".
             -> Measured
applyGCStats (Just end) (Just start) m = m {
    measAllocated          = diff bytesAllocated
  , measNumGcs             = diff numGcs
  , measBytesCopied        = diff bytesCopied
  , measMutatorWallSeconds = diff mutatorWallSeconds
  , measMutatorCpuSeconds  = diff mutatorCpuSeconds
  , measGcWallSeconds      = diff gcWallSeconds
  , measGcCpuSeconds       = diff gcCpuSeconds
  } where diff f = f end - f start
applyGCStats _ _ m = m

-- | Apply the difference between two sets of GC statistics to a
-- measurement.
applyGCStatistics :: Maybe GCStatistics
                  -- ^ Statistics gathered at the __end__ of a run.
                  -> Maybe GCStatistics
                  -- ^ Statistics gathered at the __beginning__ of a run.
                  -> Measured
                  -- ^ Value to \"modify\".
                  -> Measured
applyGCStatistics (Just end) (Just start) m = m {
    measAllocated          = diff gcStatsBytesAllocated
  , measNumGcs             = diff gcStatsNumGcs
  , measBytesCopied        = diff gcStatsBytesCopied
  , measMutatorWallSeconds = diff gcStatsMutatorWallSeconds
  , measMutatorCpuSeconds  = diff gcStatsMutatorCpuSeconds
  , measGcWallSeconds      = diff gcStatsGcWallSeconds
  , measGcCpuSeconds       = diff gcStatsGcCpuSeconds
  } where diff f = f end - f start
applyGCStatistics _ _ m = m

-- | Apply the difference between two sets of rusage statistics to a
-- measurement.
applyRUStatistics :: Maybe RUsage
                  -- ^ Statistics gathered at the __end__ of a run.
                  -> Maybe RUsage
                  -- ^ Statistics gathered at the __beginning__ of a run.
                  -> Measured
                  -- ^ Value to \"modify\".
                  -> Measured
applyRUStatistics (Just end) (Just start) m = m {
    measUtime   = diff ruUtime
  , measStime   = diff ruStime
  , measMaxrss  = ruMaxrss end
  , measMinflt  = diff ruMinflt
  , measMajflt  = diff ruMajflt
  , measNvcsw   = diff ruNvcsw
  , measNivcsw  = diff ruNivcsw
  } where diff f = f end - f start
applyRUStatistics _ _ m = m

ptrToTimeRecord :: Ptr Word8 -> IO (TimeRecord 'Absolute)
ptrToTimeRecord ptr =
    TimeRecord <$> (ClockTime <$> peek (castPtr ptr))
               <*> (CpuTime <$> peek (castPtr ptr `plusPtr` 8))
               <*> (Cycles <$> peek (castPtr ptr `plusPtr` 16))
{-# INLINE ptrToTimeRecord #-}

-- | Set up time measurement.
foreign import ccall unsafe "gauge_inittime" initializeTime :: IO ()

-- | Read the CPU cycle counter.
foreign import ccall unsafe "gauge_rdtsc" getCycles :: IO Word64

-- | Return the current wallclock time, in seconds since some
-- arbitrary time.
--
-- You /must/ call 'initializeTime' once before calling this function!
foreign import ccall unsafe "gauge_gettime" getTime :: IO Double

-- | Return the amount of elapsed CPU time, combining user and kernel
-- (system) time into a single measure.
foreign import ccall unsafe "gauge_getcputime" getCPUTime :: IO Double

-- | Record clock, cpu and cycles in one structure
foreign import ccall unsafe "gauge_record" getRecordPtr :: Ptr Word8 -> IO ()

#ifndef mingw32_HOST_OS
-- rusage accessor functions
-- XXX All fields of rusage may not be available on MinGW
foreign import ccall unsafe "gauge_getrusage" c_getRUsage :: IO (Ptr RUsage)

foreign import ccall unsafe "gauge_getrusage_utime" c_getRUutime
    :: Ptr RUsage -> IO CLong
foreign import ccall unsafe "gauge_getrusage_stime" c_getRUstime
    :: Ptr RUsage -> IO CLong
foreign import ccall unsafe "gauge_getrusage_maxrss" c_getRUmaxrss
    :: Ptr RUsage -> IO CLong
foreign import ccall unsafe "gauge_getrusage_minflt" c_getRUminflt
    :: Ptr RUsage -> IO CLong
foreign import ccall unsafe "gauge_getrusage_majflt" c_getRUmajflt
    :: Ptr RUsage -> IO CLong
foreign import ccall unsafe "gauge_getrusage_nvcsw" c_getRUnvcsw
    :: Ptr RUsage -> IO CLong
foreign import ccall unsafe "gauge_getrusage_nivcsw" c_getRUnivcsw
    :: Ptr RUsage -> IO CLong
#endif
