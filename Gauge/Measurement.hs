{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns, CPP, ScopedTypeVariables #-}

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
    , Time.getTime
    , Time.getCPUTime
    , Time.ClockTime(..)
    , Time.CpuTime(..)
    , Time.Cycles(..)
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
    ) where

import Gauge.Time (MicroSeconds(..), microSecondsToDouble, nanoSecondsToDouble)
import Control.DeepSeq (NFData(rnf))
import Control.Monad (when, unless)
import Data.Data (Data, Typeable)
import Data.Int (Int64)
import Gauge.ListMap (Map, fromList)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Text.Printf (printf)
import qualified Data.List as List
import qualified Gauge.ListMap as Map

import           Gauge.Source.RUsage (RUsage)
import qualified Gauge.Source.RUsage as RUsage

import qualified Gauge.Source.Time as Time
import qualified Gauge.Source.GC as GC

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

    , measAllocated          :: !Word64
      -- ^ __(GC)__ Number of bytes allocated.  Access using 'fromInt'.
    , measNumGcs             :: !Word64
      -- ^ __(GC)__ Number of garbage collections performed.  Access
      -- using 'fromInt'.
    , measBytesCopied        :: !Word64
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
  , ("allocated",          ( fmap fromIntegral . fromWord . measAllocated
                           , show . rnd
                           , "(+RTS -T) bytes allocated"))
  , ("numGcs",             ( fmap fromIntegral . fromWord . measNumGcs
                           , show . rnd
                           , "(+RTS -T) number of garbage collections"))
  , ("bytesCopied",        ( fmap fromIntegral . fromWord . measBytesCopied
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

initializeTime :: IO ()
initializeTime = Time.initialize

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

    , measNumGcs             = w measNumGcs
    , measBytesCopied        = w measBytesCopied
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
fromWord i | i == maxBound = Nothing
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

class MeasureDiff w where
    measureDiff :: w 'Time.Absolute -> w 'Time.Absolute -> w 'Time.Differential

instance MeasureDiff Time.ClockTime where
    measureDiff (Time.ClockTime end) (Time.ClockTime start)
        | end > start = Time.ClockTime d
        | otherwise   = Time.ClockTime 0
      where d = end - start
instance MeasureDiff Time.CpuTime where
    measureDiff (Time.CpuTime end) (Time.CpuTime start)
        | end > start = Time.CpuTime d
        | otherwise   = Time.CpuTime 0
      where d = end - start
instance MeasureDiff Time.Cycles where
    measureDiff (Time.Cycles end) (Time.Cycles start)
        | end > start = Time.Cycles d
        | otherwise   = Time.Cycles 0
      where d = end - start
instance MeasureDiff Time.TimeRecord where
    measureDiff (Time.TimeRecord a1 b1 c1) (Time.TimeRecord a2 b2 c2) =
        Time.TimeRecord (measureDiff a1 a2)
                        (measureDiff b1 b2)
                        (measureDiff c1 c2)

#ifdef GAUGE_MEASURE_TIME_NEW
measureTime :: IO () -> IO (Time.TimeRecord 'Time.Differential)
measureTime f = do
    ((), start, end) <- Time.withMetrics f
    pure $! measureDiff end start
#else
measureTime :: IO () -> IO (Double, Double, Word64)
measureTime f = do
    startTime  <- Time.getTime
    startCpu   <- Time.getCPUTime
    (Time.Cycles startCycle) <- Time.getCycles
    f
    endTime  <- Time.getTime
    endCpu   <- Time.getCPUTime
    (Time.Cycles endCycle) <- Time.getCycles
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
#ifdef GAUGE_MEASURE_TIME_NEW
    ((Time.TimeRecord time cpuTime cycles, startRUsage, endRUsage), gcStats) <- GC.withMetrics $ RUsage.with RUsage.Self $ measureTime act
#else
    (((time, cpuTime, cycles), startRUsage, endRUsage), gcStats) <- GC.withMetrics $ RUsage.with RUsage.Self $ measureTime act
#endif
    return $! applyGCStatistics gcStats
           $  applyRUStatistics endRUsage startRUsage
           $  measured { measTime    = outTime time
                       , measCpuTime = outCputime cpuTime
                       , measCycles  = outCycles cycles
                       , measIters   = iters
                       }
  where
#ifdef GAUGE_MEASURE_TIME_NEW
    outTime (Time.ClockTime w)  = fromIntegral w / 1.0e9
    outCputime (Time.CpuTime w) = fromIntegral w / 1.0e9
    outCycles (Time.Cycles w)   = fromIntegral w
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

    , measUtime              = maxBound
    , measStime              = maxBound
    , measMaxrss             = maxBound
    , measMinflt             = maxBound
    , measMajflt             = maxBound
    , measNvcsw              = maxBound
    , measNivcsw             = maxBound

    , measAllocated          = maxBound
    , measNumGcs             = maxBound
    , measBytesCopied        = maxBound
    , measMutatorWallSeconds = bad
    , measMutatorCpuSeconds  = bad
    , measGcWallSeconds      = bad
    , measGcCpuSeconds       = bad
    } where bad = -1/0

-- | Apply the difference between two sets of GC statistics to a
-- measurement.
applyGCStatistics :: Maybe GC.Metrics
                  -> Measured
                  -> Measured
applyGCStatistics (Just stats) m = m
    { measAllocated          = GC.allocated stats
    , measNumGcs             = GC.numGCs stats
    , measBytesCopied        = GC.copied stats
    , measMutatorWallSeconds = nanoSecondsToDouble $ GC.mutWallSeconds stats
    , measMutatorCpuSeconds  = nanoSecondsToDouble $ GC.mutCpuSeconds stats
    , measGcWallSeconds      = nanoSecondsToDouble $ GC.gcWallSeconds stats
    , measGcCpuSeconds       = nanoSecondsToDouble $ GC.gcCpuSeconds stats
    }
applyGCStatistics Nothing m = m

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
                           , measNvcsw   = diff RUsage.nVoluntaryContextSwitch
                           , measNivcsw  = diff RUsage.nInvoluntaryContextSwitch
                           }
    | otherwise        = m
 where diff f = f end - f start
       diffTV f =
            let RUsage.TimeVal (MicroSeconds endms) = f end
                RUsage.TimeVal (MicroSeconds startms) = f start
             in MicroSeconds (if endms > startms then endms - startms else 0)
