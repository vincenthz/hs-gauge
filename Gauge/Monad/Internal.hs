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
    , Crit(..)
    ) where

-- Temporary: to support pre-AMP GHC 7.8.4:
import Control.Applicative

import Foundation.Monad
import Foundation.Monad.Reader
import Gauge.Types (Config)
import Data.IORef (IORef)
import System.Random.MWC (GenIO)
import Prelude

data Crit = Crit {
    config   :: !Config
  , gen      :: !(IORef (Maybe GenIO))
  , overhead :: !(IORef (Maybe Double))
  }

-- | The monad in which most gauge code executes.
newtype Gauge a = Gauge {
      runGauge :: ReaderT Crit IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch) -- , MonadBracket)

instance MonadReader Gauge where
    type ReaderContext Gauge = Config
    ask = config `fmap` Gauge ask

instance MonadBracket Gauge where
    generalBracket acq cleanup cleanupExcept innerAction = Gauge $ do
        c <- ask
        lift $ generalBracket (runReaderT (runGauge acq) c)
                              (\a b -> runReaderT (runGauge (cleanup a b)) c)
                              (\a exn -> runReaderT (runGauge (cleanupExcept a exn)) c)
                              (\a -> runReaderT (runGauge (innerAction a)) c)
