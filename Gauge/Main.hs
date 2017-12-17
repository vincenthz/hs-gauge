{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Trustworthy     #-}

-- |
-- Module      : Gauge.Main
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Wrappers for compiling and running benchmarks quickly and easily.
-- See 'defaultMain' below for an example.

module Gauge.Main
    (
    -- * Turning a suite of benchmarks into a program
      defaultMain
    , defaultMainWith
    , runMode
    -- * Running Benchmarks Interactively
    , benchmark
    , benchmarkWith
    ) where

import Control.Applicative
import Control.Monad (unless, when)
#ifdef HAVE_ANALYSIS
import Gauge.Analysis (analyseBenchmark)
#endif
import Gauge.IO.Printf (note, printError, rewindClearLine)
import Gauge.Benchmark
import Gauge.Main.Options
import Gauge.Measurement (Measured, measureAccessors_, rescale)
import Gauge.Monad (Gauge, askConfig, withConfig, gaugeIO)
import Data.List (sort)
import Data.Traversable
import System.Environment (getProgName, getArgs)
import System.Exit (ExitCode(..), exitWith)
-- import System.FilePath.Glob
import System.IO (BufferMode(..), hSetBuffering, stdout)
import System.IO.CodePage (withCP65001)
import qualified Data.Vector as V

-- | An entry point that can be used as a @main@ function.
--
-- > import Gauge.Main
-- >
-- > fib :: Int -> Int
-- > fib 0 = 0
-- > fib 1 = 1
-- > fib n = fib (n-1) + fib (n-2)
-- >
-- > main = defaultMain [
-- >        bgroup "fib" [ bench "10" $ whnf fib 10
-- >                     , bench "35" $ whnf fib 35
-- >                     , bench "37" $ whnf fib 37
-- >                     ]
-- >                    ]
defaultMain :: [Benchmark] -> IO ()
defaultMain = defaultMainWith defaultConfig

-- | Display an error message from a command line parsing failure, and
-- exit.
parseError :: String -> IO a
parseError msg = do
  _ <- printError "Error: %s\n" msg
  _ <- printError "Run \"%s --help\" for usage information\n" =<< getProgName
  exitWith (ExitFailure 64)

selectBenches :: MatchType -> [String] -> Benchmark -> IO (String -> Bool)
selectBenches matchType benches bsgroup = do
  let toRun = makeSelector matchType benches
  unless (null benches || any toRun (benchNames bsgroup)) $
    parseError "none of the specified names matches a benchmark"
  return toRun

-- | Analyse a single benchmark, printing just the time by default and all
-- stats in verbose mode.
quickAnalyse :: String -> V.Vector Measured -> Gauge ()
quickAnalyse desc meas = do
  Config{..} <- askConfig
  let accessors =
        if verbosity == Verbose
        then measureAccessors_
        else filter (("time" ==)  . fst) measureAccessors_

  _ <- note "%s%-40s " rewindClearLine desc
  if verbosity == Verbose then gaugeIO (putStrLn "") else return ()
  _ <- traverse
        (\(k, (a, s, _)) -> reportStat a s k)
        accessors
  _ <- note "\n"
  pure ()

  where

  reportStat accessor sh msg =
    when (not $ V.null meas) $
      let val = (accessor . rescale) $ V.last meas
       in maybe (return ()) (\x -> note "%-20s %-10s\n" msg (sh x)) val

-- | Run a benchmark interactively with supplied config, and analyse its
-- performance.
benchmarkWith :: Config -> Benchmarkable -> IO ()
benchmarkWith cfg bm =
  withConfig cfg $
    runBenchmark (const True) (Benchmark "function" bm) quickAnalyse

-- | Run a benchmark interactively with default config, and analyse its
-- performance.
benchmark :: Benchmarkable -> IO ()
benchmark = benchmarkWith defaultConfig

-- | An entry point that can be used as a @main@ function, with
-- configurable defaults.
--
-- Example:
--
-- > import Gauge.Main.Options
-- > import Gauge.Main
-- >
-- > myConfig = defaultConfig {
-- >              -- Do not GC between runs.
-- >              forceGC = False
-- >            }
-- >
-- > main = defaultMainWith myConfig [
-- >          bench "fib 30" $ whnf fib 30
-- >        ]
--
-- If you save the above example as @\"Fib.hs\"@, you should be able
-- to compile it as follows:
--
-- > ghc -O --make Fib
--
-- Run @\"Fib --help\"@ on the command line to get a list of command
-- line options.
defaultMainWith :: Config
                -> [Benchmark]
                -> IO ()
defaultMainWith defCfg bs = withCP65001 $ do
    args <- getArgs
    let (cfg, extra) = parseWith defCfg args
#ifdef HAVE_ANALYSIS
    let cfg' = cfg
#else
    let cfg' = cfg {quickMode = True}
#endif
    runMode (mode cfg') cfg' extra bs

-- | Run a set of 'Benchmark's with the given 'Mode'.
--
-- This can be useful if you have a 'Mode' from some other source (e.g. from a
-- one in your benchmark driver's command-line parser).
runMode :: Mode -> Config -> [String] -> [Benchmark] -> IO ()
runMode wat cfg benches bs =
  -- TBD: This has become messy. We use mode as well as cfg options for the
  -- same purpose It is possible to specify multiple exclusive options.  We
  -- need to handle the exclusive options in a better way.
  case wat of
    List    -> mapM_ putStrLn . sort . concatMap benchNames $ bs
    Version -> putStrLn versionInfo
    Help    -> putStrLn describe
    DefaultMode ->
      case measureOnly cfg of
        Just outfile -> runWithConfig runBenchmark (\_ r ->
                          gaugeIO (writeFile outfile (show r)))
        Nothing ->
          case iters cfg of
          Just nbIters -> runWithConfig runBenchmarkIters nbIters
          Nothing ->
            case quickMode cfg of
              True  -> runWithConfig runBenchmark quickAnalyse
              False ->
#ifdef HAVE_ANALYSIS
                  runWithConfig runBenchmark analyseBenchmark
#else
                  runWithConfig runBenchmark quickAnalyse
#endif
  where bsgroup = BenchGroup "" bs
        runWithConfig f arg = do
          hSetBuffering stdout NoBuffering
          selector <- selectBenches (match cfg) benches bsgroup
          withConfig cfg $ f selector bsgroup arg
