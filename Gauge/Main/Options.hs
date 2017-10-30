{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, RecordWildCards #-}

-- |
-- Module      : Gauge.Main.Options
-- Copyright   : (c) 2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Benchmarking command-line configuration.

module Gauge.Main.Options
    ( defaultConfig
    , parseWith
    , describe
    , versionInfo
    ) where

-- Temporary: to support pre-AMP GHC 7.8.4:
import Data.Monoid

import Gauge.Analysis (validateAccessors)
import Gauge.Types (Config(..), Verbosity(..), Mode(..), DisplayMode(..), MatchType(..))
--import Gauge.Types (Config(..), Verbosity(..), measureAccessors, measureKeys, Mode(..), MatchType(..))
import Data.Char (isSpace, toLower)
import Data.List (foldl')
import Data.Version (showVersion)
import System.Console.GetOpt
import Paths_gauge (version)
import Statistics.Types (mkCL,cl95)
import Prelude

-- | Default benchmarking configuration.
defaultConfig :: Config
defaultConfig = Config
    { confInterval = cl95
    , forceGC      = True
    , timeLimit    = 5
    , resamples    = 1000
    , regressions  = []
    , rawDataFile  = Nothing
    , reportFile   = Nothing
    , csvFile      = Nothing
    , jsonFile     = Nothing
    , junitFile    = Nothing
    , verbosity    = Normal
    , template     = "default"
    , iters        = Nothing
    , match        = Prefix
    , mode         = DefaultMode
    , displayMode  = StatsTable
    }

parseWith :: Config
            -- ^ Default configuration to use
          -> [String]
            -- ^ Program Argument
          -> (Config, [String])
parseWith start argv =
    case getOpt Permute opts argv of
        (o,n,[]  ) -> (foldl' (flip id) start o, n)
        (_,_,errs) -> optionError (concat errs ++ usageInfo header opts)

opts :: [OptDescr (Config -> Config)]
opts =
    [ Option "I" ["ci"]         (ReqArg setCI "CI") "Confidence interval"
    , Option "G" ["no-gc"]      (NoArg setNoGC)     "Do not collect garbage between iterations"
    , Option "L" ["time-limit"] (ReqArg setTimeLimit "SECS") "Time limit to run a benchmark"
    , Option ""  ["resamples"]  (ReqArg setResamples "COUNT") "Number of boostrap resamples to perform"
    , Option ""  ["regress"]    (ReqArg setRegressions "RESP:PRED..") "Regressions to perform"
    , Option ""  ["raw"]        (fileArg setRaw) "File to write raw data to"
    , Option "o" ["output"]     (fileArg setOutput) "File to write report to"
    , Option ""  ["csv"]        (fileArg setCSV) "File to write CSV summary to"
    , Option ""  ["json"]       (fileArg setJSON) "File to write JSON summary to"
    , Option ""  ["junit"]      (fileArg setJUnit) "File to write JUnit summary to"
    , Option "v" ["verbosity"]  (ReqArg setVerbosity "LEVEL") "Verbosity level"
    , Option "t" ["template"]   (fileArg setTemplate) "Template to use for report"
    , Option "n" ["iters"]      (ReqArg setIters "ITERS") "Run benchmarks, don't analyse"
    , Option "m" ["match"]      (ReqArg setMatch "MATCH") "How to match benchmark names: prefix, glob, pattern, or ipattern"
    , Option "l" ["list"]       (NoArg $ setMode List) "List benchmarks"
    , Option ""  ["version"]    (NoArg $ setMode Version) "Show version info"
    , Option "s" ["small"]      (NoArg $ setDisplayMode Condensed) "Set benchmark display to the minimum useful information"
    , Option "h" ["help"]       (NoArg $ setMode Help) "Show help"
    ]
  where
    fileArg f = ReqArg f "FILE"
    setCI s v = v { confInterval = mkCL (range 0.001 0.999 s) }
    setNoGC v = v { forceGC = False }
    setTimeLimit s v = v { timeLimit = range 0.1 86400 s }
    setResamples s v = v { resamples = range 1 1000000 s }
    setRegressions s v = v { regressions = regressParams s : regressions v }
    setRaw f v = v { rawDataFile = Just f }
    setOutput f v = v { reportFile = Just f }
    setCSV f v = v { csvFile = Just f }
    setJSON f v = v { jsonFile = Just f }
    setJUnit f v = v { junitFile = Just f }
    setVerbosity s v = v { verbosity = toEnum (range 0 2 s) }
    setTemplate f v = v { template = f }
    setIters s v = v { iters = Just $ read s }
    setMatch s v =
        let m = case map toLower s of
                    "pfx"      -> Prefix
                    "prefix"   -> Prefix
                    "pattern"  -> Pattern
                    "ipattern" -> IPattern
                    _          -> optionError ("unknown match type: " <> s)
         in v { match = m }
    setMode m v = v { mode = m }
    setDisplayMode m v = v { displayMode = m }

-- FIXME
optionError :: String -> a
optionError s = error s

range :: (Show a, Read a, Ord a) => a -> a -> String -> a
range lo hi s = do
    case reads s of
        [(i, "")]
            | i >= lo && i <= hi -> i
            | otherwise          -> optionError $ show i ++ " is outside range " ++ show (lo,hi)
        _ -> optionError $ show s ++ " is not a number"

{-
Regression metrics (for use with --regress):
  time                     wall-clock time
  cpuTime                  CPU time
  cycles                   CPU cycles
  iters                    loop iterations
  allocated                (+RTS -T) bytes allocated
  numGcs                   (+RTS -T) number of garbage collections
  bytesCopied              (+RTS -T) number of bytes copied during GC
  mutatorWallSeconds       (+RTS -T) wall-clock time for mutator threads
  mutatorCpuSeconds        (+RTS -T) CPU time spent running mutator threads
  gcWallSeconds            (+RTS -T) wall-clock time spent doing GC
  gcCpuSeconds             (+RTS -T) CPU time spent doing GC
Benchmark self: FINISH

-- We sort not by name, but by likely frequency of use.
regressionHelp :: Chunk Doc
regressionHelp =
    fmap (text "Regression metrics (for use with --regress):" .$.) $
      tabulate [(text n,text d) | (n,(_,d)) <- map f measureKeys]
  where f k = (k, measureAccessors M.! k)
  -}

describe :: String
describe = usageInfo header opts

header :: String
header = "Microbenchmark suite - " <> versionInfo

-- | A string describing the version of this benchmark (really, the
-- version of gauge that was used to build it).
versionInfo :: String
versionInfo = "built with gauge " <> showVersion version

regressParams :: String -> ([String], String)
regressParams m
    | null r    = optionError "no responder specified"
    | null ps   = optionError "no predictors specified"
    | otherwise = 
        let ret = (words . map repl . drop 1 $ ps, tidy r)
        in either optionError (const ret) $ uncurry validateAccessors ret
  where
      repl ','   = ' '
      repl c     = c
      tidy       = reverse . dropWhile isSpace . reverse . dropWhile isSpace
      (r,ps)     = break (==':') m

