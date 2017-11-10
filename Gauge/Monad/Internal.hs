{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- |
-- Module      : Gauge.Monad.Internal
-- Copyright   : (c) 2009 Neil Brown
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- The environment in which most gauge code executes.
module Gauge.Monad.Internal
    (
      Gauge(..)
    , gaugeIO
    , finallyGauge
    , Crit(..)
    , askConfig
    , askCrit
    ) where

-- Temporary: to support pre-AMP GHC 7.8.4:
import Control.Applicative
import Control.Exception
import Control.Monad (ap)

import Gauge.Types (Config)
import Data.IORef (IORef)
import System.Random.MWC (GenIO)
import Prelude

data Crit = Crit
    { config   :: !Config
    , gen      :: !(IORef (Maybe GenIO))
    }

-- | The monad in which most gauge code executes.
newtype Gauge a = Gauge { runGauge :: Crit -> IO a }

instance Functor Gauge where
    fmap f a = Gauge $ \r -> f <$> runGauge a r
instance Applicative Gauge where
    pure = Gauge . const . pure
    (<*>) = ap
instance Monad Gauge where
    return    = pure
    ma >>= mb = Gauge $ \r -> runGauge ma r >>= \a -> runGauge (mb a) r

askConfig :: Gauge Config
askConfig = Gauge (pure . config)

askCrit :: Gauge Crit
askCrit = Gauge pure

gaugeIO :: IO a -> Gauge a
gaugeIO = Gauge . const

finallyGauge :: Gauge a -> Gauge b -> Gauge a
finallyGauge f g = Gauge $ \crit -> do
    finally (runGauge f crit) (runGauge g crit)
