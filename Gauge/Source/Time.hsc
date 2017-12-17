-- |
-- Module      : Gauge.Source.Time
-- Copyright   : (c) 2017 Vincent Hanquez
--
-- Various system time gathering methods
--
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Gauge.Source.Time
    ( initialize
    , ClockTime(..)
    , CpuTime(..)
    , Cycles(..)
    , TimeRecord(..)
    , MeasurementType(..)
    , getCycles
    , getTime
    , getCPUTime
    , getMetrics
    , withMetrics
    ) where

#include "gauge-time.h"

import Control.Applicative
import Data.Word (Word64)
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc (alloca, allocaBytes)

data MeasurementType = Differential | Absolute

newtype ClockTime (ty :: MeasurementType) = ClockTime Word64
    deriving (Eq, Storable)
newtype CpuTime (ty :: MeasurementType) = CpuTime Word64
    deriving (Eq, Storable)
newtype Cycles (ty :: MeasurementType) = Cycles Word64
    deriving (Eq, Storable)

data TimeRecord w = TimeRecord
    {-# UNPACK #-} !(ClockTime w)
    {-# UNPACK #-} !(CpuTime w)
    {-# UNPACK #-} !(Cycles w)

instance Storable (TimeRecord w) where
    alignment _ = 8
    sizeOf _ = sizeTimeRecord
    peek p = TimeRecord <$> (#peek struct gauge_time, clock_nanosecs) p
                        <*> (#peek struct gauge_time, cpu_nanosecs) p
                        <*> (#peek struct gauge_time, rdtsc) p
    poke p (TimeRecord clock cpu rdtsc) = do
        (#poke struct gauge_time, clock_nanosecs) p clock
        (#poke struct gauge_time, cpu_nanosecs  ) p cpu
        (#poke struct gauge_time, rdtsc         ) p rdtsc

sizeTimeRecord :: Int
sizeTimeRecord = #const sizeof(struct gauge_time)

getMetrics :: IO (TimeRecord 'Absolute)
getMetrics = alloca $ \ptr -> getRecordPtr ptr >> peek ptr

withMetrics :: IO a -> IO (a, TimeRecord 'Absolute, TimeRecord 'Absolute)
withMetrics f = allocaBytes (sizeTimeRecord * 2) $ \ptr -> do
    let ptr2 = ptr `plusPtr` sizeTimeRecord
    getRecordPtr ptr
    a <- f
    getRecordPtr ptr2
    (,,) <$> pure a <*> peek ptr <*> peek ptr2

-- | Set up time measurement.
foreign import ccall unsafe "gauge_inittime" initialize :: IO ()

-- | Read the CPU cycle counter.
foreign import ccall unsafe "gauge_rdtsc" getCycles :: IO (Cycles 'Absolute)

-- | Return the current wallclock time, in seconds since some
-- arbitrary time.
--
-- You /must/ call 'initializeTime' once before calling this function!
foreign import ccall unsafe "gauge_gettime" getTime :: IO Double

-- | Return the amount of elapsed CPU time, combining user and kernel
-- (system) time into a single measure.
foreign import ccall unsafe "gauge_getcputime" getCPUTime :: IO Double

-- | Record clock, cpu and cycles in one structure
foreign import ccall unsafe "gauge_record" getRecordPtr :: Ptr (TimeRecord 'Absolute) -> IO ()
