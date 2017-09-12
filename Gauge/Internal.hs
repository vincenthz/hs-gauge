{-# LANGUAGE BangPatterns, RecordWildCards #-}
-- |
-- Module      : Criterion
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
    , runOne
    , runFixedIters
    ) where

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Monad (foldM, forM_, void, when)
import Control.Monad.Catch (MonadMask, finally)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Except
import Data.Int (Int64)
import Gauge.Analysis (analyseSample, noteOutliers)
import Gauge.IO.Printf (note, printError, prolix)
import Gauge.Measurement (runBenchmark, runBenchmarkable_, secs)
import Gauge.Monad (Criterion)
import Gauge.Types hiding (measure)
import qualified Data.Map as Map
import qualified Data.Vector as V
import Statistics.Types (Estimate(..),ConfInt(..),confidenceInterval,cl95,confidenceLevel)
import Text.Printf (printf)

-- | Run a single benchmark.
runOne :: Int -> String -> Benchmarkable -> Criterion DataRecord
runOne i desc bm = do
  Config{..} <- ask
  (meas,timeTaken) <- liftIO $ runBenchmark bm timeLimit
  when (timeTaken > timeLimit * 1.25) .
    void $ prolix "measurement took %s\n" (secs timeTaken)
  return (Measurement i desc meas)

-- | Analyse a single benchmark.
analyseOne :: Int -> String -> V.Vector Measured -> Criterion DataRecord
analyseOne i desc meas = do
  Config{..} <- ask
  _ <- prolix "analysing with %d resamples\n" resamples
  erp <- runExceptT $ analyseSample i desc meas
  case erp of
    Left err -> printError "*** Error: %s\n" err
    Right rpt@Report{..} -> do
      let SampleAnalysis{..} = reportAnalysis
          OutlierVariance{..} = anOutlierVar
          wibble = case ovEffect of
                     Unaffected -> "unaffected" :: String
                     Slight -> "slightly inflated"
                     Moderate -> "moderately inflated"
                     Severe -> "severely inflated"
          (builtin, others) = splitAt 1 anRegress
      let r2 n = printf "%.3f R\178" n
      forM_ builtin $ \Regression{..} ->
        case Map.lookup "iters" regCoeffs of
          Nothing -> return ()
          Just t  -> bs secs "time" t >> bs r2 "" regRSquare
      bs secs "mean" anMean
      bs secs "std dev" anStdDev
      forM_ others $ \Regression{..} -> do
        _ <- bs r2 (regResponder ++ ":") regRSquare
        forM_ (Map.toList regCoeffs) $ \(prd,val) ->
          bs (printf "%.3g") ("  " ++ prd) val
      --writeCsv
      --  (desc,
      --   estPoint anMean,   fst $ confidenceInterval anMean,   snd $ confidenceInterval anMean,
      --   estPoint anStdDev, fst $ confidenceInterval anStdDev, snd $ confidenceInterval anStdDev
      -- )
      when (verbosity == Verbose || (ovEffect > Slight && verbosity > Quiet)) $ do
        when (verbosity == Verbose) $ noteOutliers reportOutliers
        _ <- note "variance introduced by outliers: %d%% (%s)\n"
             (round (ovFraction * 100) :: Int) wibble
        return ()
      _ <- note "\n"
      return (Analysed rpt)
      where bs :: (Double -> String) -> String -> Estimate ConfInt Double -> Criterion ()
            bs f metric e@Estimate{..} =
              note "%-20s %-10s (%s .. %s%s)\n" metric
                   (f estPoint) (f $ fst $ confidenceInterval e) (f $ snd $ confidenceInterval e)
                   (let cl = confIntCL estError
                        str | cl == cl95 = ""
                            | otherwise  = printf ", ci %.3f" (confidenceLevel cl)
                    in str
                   )


-- | Run a single benchmark and analyse its performance.
runAndAnalyseOne :: Int -> String -> Benchmarkable -> Criterion DataRecord
runAndAnalyseOne i desc bm = do
  Measurement _ _ meas <- runOne i desc bm
  analyseOne i desc meas

-- | Run, and analyse, one or more benchmarks.
runAndAnalyse :: (String -> Bool) -- ^ A predicate that chooses
                                  -- whether to run a benchmark by its
                                  -- name.
              -> Benchmark
              -> Criterion ()
runAndAnalyse select bs = do
  -- The type we write to the file is ReportFileContents, a triple.
  -- But here we ASSUME that the tuple will become a JSON array.
  -- This assumption lets us stream the reports to the file incrementally:
  --liftIO $ hPutStr handle $ "[ \"" ++ headerRoot ++ "\", " ++
  --                           "\"" ++ critVersion ++ "\", [ "

  for select bs $ \idx desc bm -> do
    _ <- note "benchmarking %s\n" desc
    Analysed _ <- runAndAnalyseOne idx desc bm
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
              -> Criterion ()
runFixedIters iters select bs =
  for select bs $ \_idx desc bm -> do
    _ <- note "benchmarking %s\n" desc
    liftIO $ runBenchmarkable_ bm iters

-- | Iterate over benchmarks.
for :: (MonadMask m, MonadIO m) => (String -> Bool) -> Benchmark
    -> (Int -> String -> Benchmarkable -> m ()) -> m ()
for select bs0 handle = go (0::Int) ("", bs0) >> return ()
  where
    go !idx (pfx, Environment mkenv cleanenv mkbench)
      | shouldRun pfx mkbench = do
        e <- liftIO $ do
          ee <- mkenv
          evaluate (rnf ee)
          return ee
        go idx (pfx, mkbench e) `finally` liftIO (cleanenv e)
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
junit :: [Report] -> Criterion ()
junit rs
  = do junitOpt <- asks junitFile
       case junitOpt of
         Just fn -> liftIO $ writeFile fn msg
         Nothing -> return ()
  where
    msg = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
          printf "<testsuite name=\"Criterion benchmarks\" tests=\"%d\">\n"
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
