{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Gauge.Monad
-- Copyright   : (c) 2009 Neil Brown
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- The environment in which most gauge code executes.
module Gauge.Monad
    (
      Gauge
    , askConfig
    , gaugeIO
    , withConfig
    , finallyGauge
    ) where

import Gauge.Monad.Internal (Gauge(..), Crit(..), finallyGauge, askConfig, gaugeIO)
import Gauge.Types (Config)
import Data.IORef (newIORef)

-- | Run a 'Gauge' action with the given 'Config'.
withConfig :: Config -> Gauge a -> IO a
withConfig cfg act = do
  g <- newIORef Nothing
  runGauge act (Crit cfg g)
