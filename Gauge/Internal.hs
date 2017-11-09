{-# LANGUAGE BangPatterns, RecordWildCards #-}
-- |
-- Module      : Gauge.Internal
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Core benchmarking code.

module Gauge.Internal
    (
      runQuick
    , runWithAnalysis
    , runWithAnalysisInteractive
    , runOnly
    , runFixedIters
    , quickAnalyse
    , benchmark
    , benchmarkWith
    ) where

import Control.DeepSeq (rnf)
import Control.Exception (bracket, catch, evaluate)
import Control.Monad (foldM, void, when)
import Data.Int (Int64)
import Data.Maybe (fromJust, isJust)
import Gauge.IO.Printf (note, prolix, rewindClearLine)
import Gauge.Main.Options (defaultConfig)
import Gauge.Measurement (runBenchmark, runBenchmarkable_, initializeTime)
import Gauge.Monad (Gauge, finallyGauge, askConfig, gaugeIO, withConfig)
import Gauge.Types
import System.Directory (canonicalizePath, getTemporaryDirectory, removeFile)
import System.IO (hClose, hSetBuffering, BufferMode(..), openTempFile, stdout)
import qualified Data.Vector as V
import System.Process (callProcess)

withSystemTempFile
  :: String   -- ^ File name template
  -> (FilePath -> IO a) -- ^ Callback that can use the file
  -> IO a
withSystemTempFile template action = do
  tmpDir <- getTemporaryDirectory >>= canonicalizePath
  withTempFile tmpDir
  where
  withTempFile tmpDir = bracket
    (openTempFile tmpDir template >>= \(f, h) -> hClose h >> return f)
    (\name -> ignoringIOErrors (removeFile name))
    (action)
  ignoringIOErrors act = act `catch` (\e -> const (return ()) (e :: IOError))

-- | Run a single benchmark measurement only in a separate process.
runBenchmarkWith :: String -> String -> Double -> Bool
                 -> IO (V.Vector Measured, Double)
runBenchmarkWith prog desc tlimit quick =
    withSystemTempFile "gauge-quarantine" $ \file -> do
      -- XXX This is dependent on option names, if the option names change this
      -- will break.
      callProcess prog (["--time-limit", show tlimit
                          , "--measure-only", file, desc
                         ] ++ if quick then ["--quick"] else [])
      readFile file >>= return . read

-- | Run a single benchmark.
runOne :: String -> Benchmarkable -> Maybe FilePath
       -> Gauge (V.Vector Measured)
runOne desc bm file = do
  Config{..} <- askConfig
  r@(meas, timeTaken) <-
    case measureWith of
      Just prog -> gaugeIO $ runBenchmarkWith prog desc timeLimit quickMode
      Nothing -> gaugeIO $
        if quickMode
        then runBenchmark bm 30 2 0
        else runBenchmark bm 30 10 timeLimit
  case file of
    Just f -> gaugeIO $ writeFile f (show r)
    Nothing ->
      when (timeTaken > timeLimit * 1.25) .
        void $ prolix "measurement took %s\n" (secs timeTaken)
  return meas

runOnly :: FilePath -> (String -> Bool) -> Benchmark -> Gauge ()
runOnly outfile select bs =
  for select bs $ \_ desc bm -> runOne desc bm (Just outfile) >> return ()

-- | Run a single benchmark and analyse its performance.
runAndAnalyseOne
  :: (String -> V.Vector Measured -> Gauge a)
  -> String
  -> Benchmarkable
  -> Gauge a
runAndAnalyseOne analyse desc bm = do
  _ <- note "benchmarking %s" desc
  runOne desc bm Nothing >>= analyse desc

runWithAnalysis
  :: (String -> V.Vector Measured -> Gauge a)
  -> (String -> Bool)
  -> Benchmark
  -> Gauge ()
runWithAnalysis analyse select bs = do
  gaugeIO $ hSetBuffering stdout NoBuffering
  for select bs $ \_ desc bm -> do
    _ <- runAndAnalyseOne analyse desc bm
    return ()

-- | Run a benchmark interactively, analyse its performance, and
-- return the analysis.
runWithAnalysisInteractive
  :: (String -> V.Vector Measured -> Gauge a)
  -> Config
  -> Benchmarkable
  -> IO a
runWithAnalysisInteractive analyse cfg bm = do
  initializeTime
  withConfig cfg $ runAndAnalyseOne analyse "function" bm

-- | Analyse a single benchmark.
quickAnalyse :: String -> V.Vector Measured -> Gauge ()
quickAnalyse desc meas = do
  Config{..} <- askConfig
  let accessors =
        if verbosity == Verbose
        then filter (("iters" /=) . fst) measureAccessors_
        else filter (("time" ==)  . fst) measureAccessors_

  _ <- note "%s%-40s " rewindClearLine desc
  if verbosity == Verbose then gaugeIO (putStrLn "") else return ()
  _ <- traverse
        (\(k, (a, s, _)) -> reportStat a s k)
        accessors
  _ <- note "\n"
  pure ()

  where

  reportStat accessor sh msg = do
    let v = V.map fromJust
            $ V.filter isJust
            $ V.map (accessor . rescale) meas
    when (not $ V.null v) $ note "%-20s %-10s\n" msg (sh (V.last v))

runQuick :: (String -> Bool) -> Benchmark -> Gauge ()
runQuick = runWithAnalysis quickAnalyse

-- | Run a benchmark interactively, and analyse its performance.
benchmark :: Benchmarkable -> IO ()
benchmark = benchmarkWith defaultConfig

-- | Run a benchmark interactively, and analyse its performance.
benchmarkWith :: Config -> Benchmarkable -> IO ()
benchmarkWith = runWithAnalysisInteractive quickAnalyse

-- XXX For consistency, this should also use a separate process when
-- --measure-with is specified.
-- | Run a benchmark without analysing its performance.
runFixedIters :: Int64            -- ^ Number of loop iterations to run.
              -> (String -> Bool) -- ^ A predicate that chooses
                                  -- whether to run a benchmark by its
                                  -- name.
              -> Benchmark
              -> Gauge ()
runFixedIters iters select bs =
  for select bs $ \_idx desc bm -> do
    _ <- note "benchmarking %s\r" desc
    gaugeIO $ runBenchmarkable_ bm iters

-- | Iterate over benchmarks.
for :: (String -> Bool)
    -> Benchmark
    -> (Int -> String -> Benchmarkable -> Gauge ())
    -> Gauge ()
for select bs0 handle = go (0::Int) ("", bs0) >> return ()
  where
    go !idx (pfx, Environment mkenv cleanenv mkbench)
      | shouldRun pfx mkbench = do
        e <- gaugeIO $ do
          ee <- mkenv
          evaluate (rnf ee)
          return ee
        go idx (pfx, mkbench e) `finallyGauge` gaugeIO (cleanenv e)
      | otherwise = return idx
    go idx (pfx, Benchmark desc b)
      | select desc' = do handle idx desc' b; return $! idx + 1
      | otherwise    = return idx
      where desc' = addPrefix pfx desc
    go idx (pfx, BenchGroup desc bs) =
      foldM go idx [(addPrefix pfx desc, b) | b <- bs]

    shouldRun pfx mkbench =
      any (select . addPrefix pfx) . benchNames . mkbench $
      error "Gauge.env could not determine the list of your benchmarks since they force the environment (see the documentation for details)"

{-
-- | Write summary JUnit file (if applicable)
junit :: [Report] -> Gauge ()
junit rs
  = do junitOpt <- asks junitFile
       case junitOpt of
         Just fn -> liftIO $ writeFile fn msg
         Nothing -> return ()
  where
    msg = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
          printf "<testsuite name=\"Gauge benchmarks\" tests=\"%d\">\n"
          (length rs) ++
          concatMap single rs ++
          "</testsuite>\n"
    single Report{..} = printf "  <testcase name=\"%s\" time=\"%f\" />\n"
               (attrEsc reportName) (estPoint $ anMean $ reportAnalysis)
    attrEsc = concatMap esc
      where
        esc '\'' = "&apos;"
        esc '"'  = "&quot;"
        esc '<'  = "&lt;"
        esc '>'  = "&gt;"
        esc '&'  = "&amp;"
        esc c    = [c]
-}
