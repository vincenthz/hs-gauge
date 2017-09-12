{-# LANGUAGE CPP, OverloadedStrings #-}

-- |
-- Module      : Gauge.IO
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Input and output actions.

module Gauge.IO
    (
      headerRoot
    , critVersion
    , ReportFileContents
    ) where

import Gauge.Types (Report(..))
import Data.List (intercalate)
import Data.Version (Version(..))
import Paths_gauge (version)

-- | The magic string we expect to start off the header.
headerRoot :: String
headerRoot = "criterio"

-- | The current version of gauge, encoded into a string that is
-- used in files.
critVersion :: String
critVersion = intercalate "." $ map show $ versionBranch version

-- | On disk we store (name,version,reports), where
--   'version' is the version of Criterion used to generate the file.
type ReportFileContents = (String,String,[Report])
