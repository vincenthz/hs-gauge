{-# LANGUAGE BangPatterns, RecordWildCards #-}
-- |
-- Module      : Gauge.Benchmark
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Core benchmarking code.

module Gauge.Benchmark
    ( runBenchmark
    , runBenchmarkable
    , runBenchmarkable_
    , runQuick
    , runWithAnalysis
    , runWithAnalysisInteractive
    , runOnly
    , runFixedIters
    , quickAnalyse
    , benchmark
    , benchmarkWith
    ) where

import Control.Applicative ((<*))
import Control.DeepSeq (rnf)
import Control.Exception (bracket, catch, evaluate, finally)
import Control.Monad (foldM, void, when)
import Data.Int (Int64)
import Data.List (unfoldr)
import Gauge.IO.Printf (note, prolix, rewindClearLine)
import Gauge.Main.Options (defaultConfig)
import Gauge.Measurement (initializeTime, measure, getTime)
import Gauge.Monad (Gauge, finallyGauge, askConfig, gaugeIO, withConfig)
import Gauge.Types (Config(..), Benchmark(..), secs, Verbosity(..),
                    measureAccessors_, rescale, addPrefix, benchNames)
import Gauge.Types (Benchmarkable(..), Measured(..))
import System.Directory (canonicalizePath, getTemporaryDirectory, removeFile)
import System.IO (hClose, hSetBuffering, BufferMode(..), openTempFile, stdout)
import System.Mem (performGC)
import qualified Data.Vector as V
import System.Process (callProcess)

-- | Take a 'Benchmarkable', number of iterations, a function to combine the
-- results of multiple iterations and a measurement function to measure the
-- stats over a number of iterations.
runBenchmarkable :: Benchmarkable
                 -> Int64
                 -> (a -> a -> a)
                 -> (IO () -> IO a)
                 -> IO a
runBenchmarkable Benchmarkable{..} i comb f
    | perRun = work >>= go (i - 1)
    | otherwise = work
  where
    go 0 result = return result
    go !n !result = work >>= go (n - 1) . comb result

    count | perRun = 1
          | otherwise = i

    work = do
        env <- allocEnv count
        let clean = cleanEnv count env
            run = runRepeatedly env count

        clean `seq` run `seq` evaluate $ rnf env

        performGC
        f run `finally` clean <* performGC
    {-# INLINE work #-}
{-# INLINE runBenchmarkable #-}

runBenchmarkable_ :: Benchmarkable -> Int64 -> IO ()
runBenchmarkable_ bm i = runBenchmarkable bm i (\() () -> ()) id
{-# INLINE runBenchmarkable_ #-}

series :: Double -> Maybe (Int64, Double)
series k = Just (truncate l, l)
  where l = k * 1.05

-- Our series starts its growth very slowly when we begin at 1, so we
-- eliminate repeated values.
squish :: (Eq a) => [a] -> [a]
squish ys = foldr go [] ys
  where go x xs = x : dropWhile (==x) xs

-- | Run a single benchmark, and return measurements collected while executing
-- it, along with the amount of time the measurement process took. The
-- benchmark will not terminate until we reach all the minimum bounds
-- specified. If the minimum bounds are satisfied, the benchmark will terminate
-- as soon as we reach any of the maximums.
runBenchmark :: Benchmarkable
             -> Int
             -- ^ Minimum sample duration to in ms.
             -> Int
             -- ^ Minimum number of samples.
             -> Double
             -- ^ Upper bound on how long the benchmarking process
             -- should take.
             -> IO (V.Vector Measured, Double)
runBenchmark bm minDuration minSamples timeLimit = do
  runBenchmarkable_ bm 1
  start <- performGC >> getTime
  let loop [] !_ _ = error "unpossible!"
      loop (iters:niters) samples acc = do
        m <- measure (runBenchmarkable bm iters) iters
        endTime <- getTime
        if samples >= minSamples &&
           measTime m >= fromIntegral minDuration / 1000 &&
           endTime - start >= timeLimit
          then do
            let !v = V.reverse (V.fromList acc)
            return (v, endTime - start)
          else if measTime m >= fromIntegral minDuration / 1000
               then loop niters (samples + 1) (m:acc)
               else loop niters samples (acc)
  loop (squish (unfoldr series 1)) 0 []

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

  reportStat accessor sh msg =
    when (not $ V.null meas) $
      let val = (accessor . rescale) $ V.last meas
       in maybe (return ()) (\x -> note "%-20s %-10s\n" msg (sh x)) val

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
