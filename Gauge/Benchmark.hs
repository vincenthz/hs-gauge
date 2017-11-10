{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
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
    (
    -- * Benchmark descriptions
      Benchmarkable(..)
    , Benchmark(..)
    -- * Benchmark construction
    , env
    , envWithCleanup
    , perBatchEnv
    , perBatchEnvWithCleanup
    , perRunEnv
    , perRunEnvWithCleanup
    , toBenchmarkable
    , bench
    , bgroup
    , addPrefix
    , benchNames
    -- ** Evaluation control
    , whnf
    , nf
    , nfIO
    , whnfIO
    -- * Running Benchmarks
    , runBenchmark
    , runBenchmarkable
    , runBenchmarkable_
    , runQuick
    , runWithAnalysis
    , runWithAnalysisInteractive
    , runOnly
    , runFixedIters
    , quickAnalyse
    -- * Running Benchmarks Interactively
    , benchmark
    , benchmarkWith
    ) where

import Control.Applicative ((<*))
import Control.DeepSeq (NFData(rnf))
import Control.Exception (bracket, catch, evaluate, finally)
import Control.Monad (foldM, void, when)
import Data.Int (Int64)
import Data.List (unfoldr)
import Gauge.IO.Printf (note, prolix, rewindClearLine)
import Gauge.Main.Options (defaultConfig)
import Gauge.Measurement (initializeTime, measure, getTime, secs,
                          measureAccessors_, rescale, Measured(..))
import Gauge.Monad (Gauge, finallyGauge, askConfig, gaugeIO, withConfig)
import Gauge.Types (Config(..), Verbosity(..))
import System.Directory (canonicalizePath, getTemporaryDirectory, removeFile)
import System.IO (hClose, hSetBuffering, BufferMode(..), openTempFile, stdout)
import System.Mem (performGC)
import qualified Data.Vector as V
import System.Process (callProcess)

-- | A pure function or impure action that can be benchmarked. The
-- 'Int64' parameter indicates the number of times to run the given
-- function or action.
data Benchmarkable = forall a . NFData a =>
    Benchmarkable
      { allocEnv :: Int64 -> IO a
      , cleanEnv :: Int64 -> a -> IO ()
      , runRepeatedly :: a -> Int64 -> IO ()
      , perRun :: Bool
      }

noop :: Monad m => a -> m ()
noop = const $ return ()
{-# INLINE noop #-}

-- | Construct a 'Benchmarkable' value from an impure action, where the 'Int64'
-- parameter indicates the number of times to run the action.
toBenchmarkable :: (Int64 -> IO ()) -> Benchmarkable
toBenchmarkable f = Benchmarkable noop (const noop) (const f) False
{-# INLINE toBenchmarkable #-}

pureFunc :: (b -> c) -> (a -> b) -> a -> Benchmarkable
pureFunc reduce f0 x0 = toBenchmarkable (go f0 x0)
  where go f x n
          | n <= 0    = return ()
          | otherwise = evaluate (reduce (f x)) >> go f x (n-1)
{-# INLINE pureFunc #-}

-- | Apply an argument to a function, and evaluate the result to weak
-- head normal form (WHNF).
whnf :: (a -> b) -> a -> Benchmarkable
whnf = pureFunc id
{-# INLINE whnf #-}

-- | Apply an argument to a function, and evaluate the result to
-- normal form (NF).
nf :: NFData b => (a -> b) -> a -> Benchmarkable
nf = pureFunc rnf
{-# INLINE nf #-}

impure :: (a -> b) -> IO a -> Int64 -> IO ()
impure strategy a = go
  where go n
          | n <= 0    = return ()
          | otherwise = a >>= (evaluate . strategy) >> go (n-1)
{-# INLINE impure #-}

-- | Perform an action, then evaluate its result to normal form.
-- This is particularly useful for forcing a lazy 'IO' action to be
-- completely performed.
nfIO :: NFData a => IO a -> Benchmarkable
nfIO = toBenchmarkable . impure rnf
{-# INLINE nfIO #-}

-- | Perform an action, then evaluate its result to weak head normal
-- form (WHNF).  This is useful for forcing an 'IO' action whose result
-- is an expression to be evaluated down to a more useful value.
whnfIO :: IO a -> Benchmarkable
whnfIO = toBenchmarkable . impure id
{-# INLINE whnfIO #-}

-- | Specification of a collection of benchmarks and environments. A
-- benchmark may consist of:
--
-- * An environment that creates input data for benchmarks, created
--   with 'env'.
--
-- * A single 'Benchmarkable' item with a name, created with 'bench'.
--
-- * A (possibly nested) group of 'Benchmark's, created with 'bgroup'.
data Benchmark where
    Environment  :: NFData env
                 => IO env -> (env -> IO a) -> (env -> Benchmark) -> Benchmark
    Benchmark    :: String -> Benchmarkable -> Benchmark
    BenchGroup   :: String -> [Benchmark] -> Benchmark

instance Show Benchmark where
    show (Environment _ _ b) = "Environment _ _" ++ show (b undefined)
    show (Benchmark d _)   = "Benchmark " ++ show d
    show (BenchGroup d _)  = "BenchGroup " ++ show d

-- | Run a benchmark (or collection of benchmarks) in the given
-- environment.  The purpose of an environment is to lazily create
-- input data to pass to the functions that will be benchmarked.
--
-- A common example of environment data is input that is read from a
-- file.  Another is a large data structure constructed in-place.
--
-- By deferring the creation of an environment when its associated
-- benchmarks need the its, we avoid two problems that this strategy
-- caused:
--
-- * Memory pressure distorted the results of unrelated benchmarks.
--   If one benchmark needed e.g. a gigabyte-sized input, it would
--   force the garbage collector to do extra work when running some
--   other benchmark that had no use for that input.  Since the data
--   created by an environment is only available when it is in scope,
--   it should be garbage collected before other benchmarks are run.
--
-- * The time cost of generating all needed inputs could be
--   significant in cases where no inputs (or just a few) were really
--   needed.  This occurred often, for instance when just one out of a
--   large suite of benchmarks was run, or when a user would list the
--   collection of benchmarks without running any.
--
-- __Creation.__ An environment is created right before its related
-- benchmarks are run.  The 'IO' action that creates the environment
-- is run, then the newly created environment is evaluated to normal
-- form (hence the 'NFData' constraint) before being passed to the
-- function that receives the environment.
--
-- __Complex environments.__ If you need to create an environment that
-- contains multiple values, simply pack the values into a tuple.
--
-- __Lazy pattern matching.__ In situations where a \"real\"
-- environment is not needed, e.g. if a list of benchmark names is
-- being generated, @undefined@ will be passed to the function that
-- receives the environment.  This avoids the overhead of generating
-- an environment that will not actually be used.
--
-- The function that receives the environment must use lazy pattern
-- matching to deconstruct the tuple, as use of strict pattern
-- matching will cause a crash if @undefined@ is passed in.
--
-- __Example.__ This program runs benchmarks in an environment that
-- contains two values.  The first value is the contents of a text
-- file; the second is a string.  Pay attention to the use of a lazy
-- pattern to deconstruct the tuple in the function that returns the
-- benchmarks to be run.
--
-- > setupEnv = do
-- >   let small = replicate 1000 (1 :: Int)
-- >   big <- map length . words <$> readFile "/usr/dict/words"
-- >   return (small, big)
-- >
-- > main = defaultMain [
-- >    -- notice the lazy pattern match here!
-- >    env setupEnv $ \ ~(small,big) -> bgroup "main" [
-- >    bgroup "small" [
-- >      bench "length" $ whnf length small
-- >    , bench "length . filter" $ whnf (length . filter (==1)) small
-- >    ]
-- >  ,  bgroup "big" [
-- >      bench "length" $ whnf length big
-- >    , bench "length . filter" $ whnf (length . filter (==1)) big
-- >    ]
-- >  ] ]
--
-- __Discussion.__ The environment created in the example above is
-- intentionally /not/ ideal.  As Haskell's scoping rules suggest, the
-- variable @big@ is in scope for the benchmarks that use only
-- @small@.  It would be better to create a separate environment for
-- @big@, so that it will not be kept alive while the unrelated
-- benchmarks are being run.
env :: NFData env =>
       IO env
    -- ^ Create the environment.  The environment will be evaluated to
    -- normal form before being passed to the benchmark.
    -> (env -> Benchmark)
    -- ^ Take the newly created environment and make it available to
    -- the given benchmarks.
    -> Benchmark
env alloc = Environment alloc noop

-- | Same as `env`, but but allows for an additional callback
-- to clean up the environment. Resource clean up is exception safe, that is,
-- it runs even if the 'Benchmark' throws an exception.
envWithCleanup
    :: NFData env
    => IO env
    -- ^ Create the environment.  The environment will be evaluated to
    -- normal form before being passed to the benchmark.
    -> (env -> IO a)
    -- ^ Clean up the created environment.
    -> (env -> Benchmark)
    -- ^ Take the newly created environment and make it available to
    -- the given benchmarks.
    -> Benchmark
envWithCleanup = Environment

-- | Create a Benchmarkable where a fresh environment is allocated for every
-- batch of runs of the benchmarkable.
--
-- The environment is evaluated to normal form before the benchmark is run.
--
-- When using 'whnf', 'whnfIO', etc. Gauge creates a 'Benchmarkable'
-- whichs runs a batch of @N@ repeat runs of that expressions. Gauge may
-- run any number of these batches to get accurate measurements. Environments
-- created by 'env' and 'envWithCleanup', are shared across all these batches
-- of runs.
--
-- This is fine for simple benchmarks on static input, but when benchmarking
-- IO operations where these operations can modify (and especially grow) the
-- environment this means that later batches might have their accuracy effected
-- due to longer, for example, longer garbage collection pauses.
--
-- An example: Suppose we want to benchmark writing to a Chan, if we allocate
-- the Chan using environment and our benchmark consists of @writeChan env ()@,
-- the contents and thus size of the Chan will grow with every repeat. If
-- Gauge runs a 1,000 batches of 1,000 repeats, the result is that the
-- channel will have 999,000 items in it by the time the last batch is run.
-- Since GHC GC has to copy the live set for every major GC this means our last
-- set of writes will suffer a lot of noise of the previous repeats.
--
-- By allocating a fresh environment for every batch of runs this function
-- should eliminate this effect.
perBatchEnv
    :: (NFData env, NFData b)
    => (Int64 -> IO env)
    -- ^ Create an environment for a batch of N runs. The environment will be
    -- evaluated to normal form before running.
    -> (env -> IO b)
    -- ^ Function returning the IO action that should be benchmarked with the
    -- newly generated environment.
    -> Benchmarkable
perBatchEnv alloc = perBatchEnvWithCleanup alloc (const noop)

-- | Same as `perBatchEnv`, but but allows for an additional callback
-- to clean up the environment. Resource clean up is exception safe, that is,
-- it runs even if the 'Benchmark' throws an exception.
perBatchEnvWithCleanup
    :: (NFData env, NFData b)
    => (Int64 -> IO env)
    -- ^ Create an environment for a batch of N runs. The environment will be
    -- evaluated to normal form before running.
    -> (Int64 -> env -> IO ())
    -- ^ Clean up the created environment.
    -> (env -> IO b)
    -- ^ Function returning the IO action that should be benchmarked with the
    -- newly generated environment.
    -> Benchmarkable
perBatchEnvWithCleanup alloc clean work
    = Benchmarkable alloc clean (impure rnf . work) False

-- | Create a Benchmarkable where a fresh environment is allocated for every
-- run of the operation to benchmark. This is useful for benchmarking mutable
-- operations that need a fresh environment, such as sorting a mutable Vector.
--
-- As with 'env' and 'perBatchEnv' the environment is evaluated to normal form
-- before the benchmark is run.
--
-- This introduces extra noise and result in reduce accuracy compared to other
-- Gauge benchmarks. But allows easier benchmarking for mutable operations
-- than was previously possible.
perRunEnv
    :: (NFData env, NFData b)
    => IO env
    -- ^ Action that creates the environment for a single run.
    -> (env -> IO b)
    -- ^ Function returning the IO action that should be benchmarked with the
    -- newly genereted environment.
    -> Benchmarkable
perRunEnv alloc = perRunEnvWithCleanup alloc noop

-- | Same as `perRunEnv`, but but allows for an additional callback
-- to clean up the environment. Resource clean up is exception safe, that is,
-- it runs even if the 'Benchmark' throws an exception.
perRunEnvWithCleanup
    :: (NFData env, NFData b)
    => IO env
    -- ^ Action that creates the environment for a single run.
    -> (env -> IO ())
    -- ^ Clean up the created environment.
    -> (env -> IO b)
    -- ^ Function returning the IO action that should be benchmarked with the
    -- newly genereted environment.
    -> Benchmarkable
perRunEnvWithCleanup alloc clean work = bm { perRun = True }
  where
    bm = perBatchEnvWithCleanup (const alloc) (const clean) work

-- | Create a single benchmark.
bench :: String                 -- ^ A name to identify the benchmark.
      -> Benchmarkable          -- ^ An activity to be benchmarked.
      -> Benchmark
bench = Benchmark

-- | Group several benchmarks together under a common name.
bgroup :: String                -- ^ A name to identify the group of benchmarks.
       -> [Benchmark]           -- ^ Benchmarks to group under this name.
       -> Benchmark
bgroup = BenchGroup

-- | Add the given prefix to a name.  If the prefix is empty, the name
-- is returned unmodified.  Otherwise, the prefix and name are
-- separated by a @\'\/\'@ character.
addPrefix :: String             -- ^ Prefix.
          -> String             -- ^ Name.
          -> String
addPrefix ""  desc = desc
addPrefix pfx desc = pfx ++ '/' : desc

-- | Retrieve the names of all benchmarks.  Grouped benchmarks are
-- prefixed with the name of the group they're in.
benchNames :: Benchmark -> [String]
benchNames (Environment _ _ b) = benchNames (b undefined)
benchNames (Benchmark d _)   = [d]
benchNames (BenchGroup d bs) = map (addPrefix d) . concatMap benchNames $ bs

-------------------------------------------------------------------------------
-- Running benchmarks
-------------------------------------------------------------------------------

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
        env0 <- allocEnv count
        let clean = cleanEnv count env0
            run = runRepeatedly env0 count

        clean `seq` run `seq` evaluate $ rnf env0

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

    shouldRun :: forall a. String -> (a -> Benchmark) -> Bool
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
