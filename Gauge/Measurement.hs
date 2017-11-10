{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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
    , measured
    , applyGCStatistics
    , secs
    , Measured(..)
    , measureKeys
    , measureAccessors_
    , validateAccessors
    , renderNames
    , rescale
      -- * Deprecated
    , getGCStats
    , applyGCStats
    ) where

import Gauge.Time (MicroSeconds(..), microSecondsToDouble)
import Control.DeepSeq (NFData(rnf))
import Control.Monad (when, unless)
import Data.Data (Data, Typeable)
import Data.Int (Int64)
import Data.Map (Map, fromList)
import Data.Word (Word64, Word8)
#ifndef mingw32_HOST_OS
import Foreign.Ptr (Ptr)
#endif
import GHC.Generics (Generic)
import GHC.Stats (GCStats(..))
#if MIN_VERSION_base(4,10,0)
import GHC.Stats (RTSStats(..), GCDetails(..))
#endif
import Foreign.Ptr
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable
import Text.Printf (printf)
import qualified Control.Exception as Exc
import qualified Data.List as List
import qualified Data.Map as Map
import qualified GHC.Stats as Stats

import           Gauge.Source.RUsage (RUsage)
import qualified Gauge.Source.RUsage as RUsage

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

    , measUtime              :: !MicroSeconds
    -- ^ User time
    , measStime              :: !MicroSeconds
    -- ^ System time
    , measMaxrss             :: !Word64
    -- ^ Maximum resident set size
    , measMinflt             :: !Word64
    -- ^ Minor page faults
    , measMajflt             :: !Word64
    -- ^ Major page faults
    , measNvcsw              :: !Word64
    -- ^ Number of voluntary context switches
    , measNivcsw             :: !Word64
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
  , ("utime",              ( Just . microSecondsToDouble . measUtime
                           , secs
                           , "user time"))
  , ("stime",              ( Just . microSecondsToDouble . measStime
                           , secs
                           , "system time"))
  , ("maxrss",             ( fmap fromIntegral . fromWord . measMaxrss
                           , show . rnd
                           , "maximum resident set size"))
  , ("minflt",             ( fmap fromIntegral . fromWord . measMinflt
                           , show . rnd
                           , "minor page faults"))
  , ("majflt",             ( fmap fromIntegral . fromWord . measMajflt
                           , show . rnd
                           , "major page faults"))
  , ("nvcsw",              ( fmap fromIntegral . fromWord . measNvcsw
                           , show . rnd
                           , "voluntary context switches"))
  , ("nivcsw",             ( fmap fromIntegral . fromWord . measNivcsw
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

    , measUtime              = ts measUtime
    , measStime              = ts measStime
    -- skip measMaxrss
    , measMinflt             = w measMinflt
    , measMajflt             = w measMajflt
    , measNvcsw              = w measNvcsw
    , measNivcsw             = w measNivcsw

    , measNumGcs             = i measNumGcs
    , measBytesCopied        = i measBytesCopied
    , measMutatorWallSeconds = d measMutatorWallSeconds
    , measMutatorCpuSeconds  = d measMutatorCpuSeconds
    , measGcWallSeconds      = d measGcWallSeconds
    , measGcCpuSeconds       = d measGcCpuSeconds
    } where
        d k = maybe k (/ iters) (fromDouble k)
        i k = maybe k (round . (/ iters)) (fromIntegral <$> fromInt k)
        w k = maybe k (round . (/ iters)) (fromIntegral <$> fromWord k)
        ts (MicroSeconds k) = MicroSeconds $ maybe k (round . (/ iters)) (fromIntegral <$> fromWord k)
        iters               = fromIntegral measIters :: Double

-- | Convert a (possibly unavailable) GC measurement to a true value.
-- If the measurement is a huge negative number that corresponds to
-- \"no data\", this will return 'Nothing'.
fromInt :: Int64 -> Maybe Int64
fromInt i | i == minBound = Nothing
          | otherwise     = Just i

-- | Convert a (possibly unavailable) GC measurement to a true value.
-- If the measurement is a huge negative number that corresponds to
-- \"no data\", this will return 'Nothing'.
fromWord :: Word64 -> Maybe Word64
fromWord i | i == minBound = Nothing
           | otherwise     = Just i

{-
-- | Convert from a true value back to the packed representation used
-- for GC measurements.
toInt :: Maybe Int64 -> Int64
toInt Nothing  = minBound
toInt (Just i) = i
-}

-- | Convert a (possibly unavailable) GC measurement to a true value.
-- If the measurement is a huge negative number that corresponds to
-- \"no data\", this will return 'Nothing'.
fromDouble :: Double -> Maybe Double
fromDouble d | isInfinite d || isNaN d = Nothing
             | otherwise               = Just d

{-
-- | Convert from a true value back to the packed representation used
-- for GC measurements.
toDouble :: Maybe Double -> Double
toDouble Nothing  = -1/0
toDouble (Just d) = d
-}

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

-- | Invokes the supplied benchmark runner function with a combiner and a
-- measurer that returns the measurement of a single iteration of an IO action.
measure :: ((Measured -> Measured -> Measured)
            -> (IO () -> IO Measured) -> IO Measured)
        -> Int64         -- ^ Number of iterations.
        -> IO Measured
measure run iters = run addResults $ \act -> do
  startStats <- getGCStatistics

#ifdef GAUGE_MEASURE_TIME_NEW
  (TimeRecord time cpuTime cycles, startRUsage, endRUsage) <- RUsage.with RUsage.Self $ measureTime act
#else
  ((time, cpuTime, cycles), startRUsage, endRUsage) <- RUsage.with RUsage.Self $ measureTime act
#endif

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
applyRUStatistics :: RUsage
                  -- ^ Statistics gathered at the __end__ of a run.
                  -> RUsage
                  -- ^ Statistics gathered at the __beginning__ of a run.
                  -> Measured
                  -- ^ Value to \"modify\".
                  -> Measured
applyRUStatistics end start m
    | RUsage.supported = m { measUtime   = diffTV RUsage.userCpuTime
                           , measStime   = diffTV RUsage.systemCpuTime
                           , measMaxrss  = RUsage.maxResidentSetSize end
                           , measMinflt  = diff RUsage.minorFault
                           , measMajflt  = diff RUsage.majorFault
                           , measNvcsw   = diff RUsage.nVolutaryContextSwitch
                           , measNivcsw  = diff RUsage.nInvolutaryContextSwitch
                           }
    | otherwise        = m
 where diff f = f end - f start
       diffTV f =
            let RUsage.TimeVal (MicroSeconds endms) = f end
                RUsage.TimeVal (MicroSeconds startms) = f start
             in MicroSeconds (if endms > startms then endms - startms else 0)

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
