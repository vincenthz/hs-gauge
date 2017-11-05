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
      runAndAnalyse
    , runAndAnalyseOne
    , runFixedIters
    ) where

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Monad (foldM, void, when)
import Data.Int (Int64)
import Gauge.Analysis (analyseBenchmark)
import Gauge.IO.Printf (note, prolix)
import Gauge.Measurement (runBenchmark, runBenchmarkable_, secs)
import Gauge.Monad (Gauge, finallyGauge, askConfig, gaugeIO)
import Gauge.Types hiding (measure)
import System.IO (hSetBuffering, BufferMode(..), stdout)
import qualified Data.Vector as V

-- | Run a single benchmark.
runOne :: Benchmarkable -> Gauge (V.Vector Measured)
runOne bm = do
  Config{..} <- askConfig
  (meas,timeTaken) <- gaugeIO $ runBenchmark bm timeLimit
  when (timeTaken > timeLimit * 1.25) .
    void $ prolix "measurement took %s\n" (secs timeTaken)
  return meas

-- | Run a single benchmark and analyse its performance.
runAndAnalyseOne :: String -> Benchmarkable -> Gauge Report
runAndAnalyseOne desc bm = do
  meas <- runOne bm
  analyseBenchmark desc meas

-- | Run, and analyse, one or more benchmarks.
runAndAnalyse :: (String -> Bool) -- ^ A predicate that chooses
                                  -- whether to run a benchmark by its
                                  -- name.
              -> Benchmark
              -> Gauge ()
runAndAnalyse select bs = do
  -- The type we write to the file is ReportFileContents, a triple.
  -- But here we ASSUME that the tuple will become a JSON array.
  -- This assumption lets us stream the reports to the file incrementally:
  --liftIO $ hPutStr handle $ "[ \"" ++ headerRoot ++ "\", " ++
  --                           "\"" ++ critVersion ++ "\", [ "

  gaugeIO $ hSetBuffering stdout NoBuffering
  for select bs $ \idx desc bm -> do
    _ <- note "benchmarking %s" desc
    _ <- runAndAnalyseOne desc bm
    return ()
    --unless (idx == 0) $
    --  liftIO $ hPutStr handle ", "
      {-
    liftIO $ L.hPut handle (Aeson.encode (rpt::Report))
    -}

  --liftIO $ hPutStr handle " ] ]\n"
  --liftIO $ hClose handle

  return ()
{-
  rpts <- liftIO $ do
    res <- readJSONReports jsonFile
    case res of
      Left err -> error $ "error reading file "++jsonFile++":\n  "++show err
      Right (_,_,rs) ->
       case mbJsonFile of
         Just _ -> return rs
         _      -> removeFile jsonFile >> return rs

  rawReport rpts
  report rpts
  json rpts
  junit rpts
  -}


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
