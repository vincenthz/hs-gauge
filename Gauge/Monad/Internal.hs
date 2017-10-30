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

import Foundation.Monad (lift)
import Foundation.Monad.Reader
import Gauge.Types (Config)
import Data.IORef (IORef)
import System.Random.MWC (GenIO)
import Prelude

data Crit = Crit
    { config   :: !Config
    , gen      :: !(IORef (Maybe GenIO))
    , overhead :: !(IORef (Maybe Double))
    }

-- | The monad in which most gauge code executes.
newtype Gauge a = Gauge { runGauge :: ReaderT Crit IO a }
    deriving (Functor, Applicative, Monad)

askConfig :: Gauge Config
askConfig = config `fmap` Gauge ask

askCrit :: Gauge Crit
askCrit = Gauge ask

gaugeIO :: IO a -> Gauge a
gaugeIO f = Gauge $ lift f

finallyGauge :: Gauge a -> Gauge b -> Gauge a
finallyGauge (Gauge f) (Gauge g) = Gauge $ do
    crit <- ask
    lift $ finally (runReaderT f crit) (runReaderT g crit)
