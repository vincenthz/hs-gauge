{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Gauge.Time
    ( MicroSeconds(..)
    , MilliSeconds(..)
    , NanoSeconds(..)
    -- * Convertion functions
    , microSecondsToDouble
    , milliSecondsToDouble
    , nanoSecondsToDouble
    , doubleToNanoSeconds
    ) where

import           Data.Typeable
import           Data.Data
import           Data.Word
import           Control.DeepSeq
import           GHC.Generics

-- | Represent a number of milliseconds.
newtype MilliSeconds = MilliSeconds Word64
    deriving (Eq, Read, Show, Typeable, Data, Generic, NFData, Enum, Bounded, Num)

-- | Represent a number of microseconds
newtype MicroSeconds = MicroSeconds Word64
    deriving (Eq, Read, Show, Typeable, Data, Generic, NFData, Enum, Bounded, Num)

-- | Represent a number of nanoseconds
newtype NanoSeconds = NanoSeconds Word64
    deriving (Eq, Read, Show, Typeable, Data, Generic, NFData, Enum, Bounded, Num)

ref_nanoseconds :: Num a => a
ref_nanoseconds = 1000000000

ref_microseconds :: Num a => a
ref_microseconds = 1000000

ref_milliseconds :: Num a => a
ref_milliseconds = 1000

microSecondsToDouble :: MicroSeconds -> Double
microSecondsToDouble (MicroSeconds w) = fromIntegral w / ref_microseconds

milliSecondsToDouble :: MilliSeconds -> Double
milliSecondsToDouble (MilliSeconds w) = fromIntegral w / ref_milliseconds

nanoSecondsToDouble :: NanoSeconds -> Double
nanoSecondsToDouble (NanoSeconds w) = fromIntegral w / ref_nanoseconds

doubleToNanoSeconds :: Double -> NanoSeconds
doubleToNanoSeconds w = NanoSeconds $ truncate (w * ref_nanoseconds)
