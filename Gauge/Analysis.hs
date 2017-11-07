{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns, DeriveDataTypeable, RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Gauge.Analysis
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Analysis code for benchmarks.

-- XXX Do we really want to expose this module to users? This is all internal.
-- Most of the exports are not even used by Gauge itself outside of this
-- module.

module Gauge.Analysis
    ( Outliers(..)
    , OutlierEffect(..)
    , OutlierVariance(..)
    , SampleAnalysis(..)
    , analyseSample
    , scale
    , analyseBenchmark
    , analyseMean
    , countOutliers
    , classifyOutliers
    , noteOutliers
    , outlierVariance
    , resolveAccessors
    , validateAccessors
    , regress
    ) where

-- Temporary: to support pre-AMP GHC 7.8.4:
import Data.Monoid

import Control.Arrow (second)
import Control.Monad (forM_, unless, when)
import Gauge.IO.Printf (note, printError, prolix, rewindClearLine)
import Gauge.Measurement (secs, threshold)
import Gauge.Monad (Gauge, getGen, getOverhead, askConfig, gaugeIO)
import Gauge.Types
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Statistics.Function (sort)
import Statistics.Quantile (weightedAvg, Sorted(..))
import Statistics.Regression (bootstrapRegress, olsRegress)
import Statistics.Resampling (Estimator(..),resample)
import Statistics.Sample (mean)
import Statistics.Sample.KernelDensity (kde)
import Statistics.Types (Sample, Estimate(..),ConfInt(..),confidenceInterval
                        ,cl95,confidenceLevel)
import System.Random.MWC (GenIO)
import Text.Printf (printf)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Statistics.Resampling.Bootstrap as B
import qualified Statistics.Types                as B
import Prelude

-- | Classify outliers in a data set, using the boxplot technique.
classifyOutliers :: Sample -> Outliers
classifyOutliers sa = U.foldl' ((. outlier) . mappend) mempty ssa
    where outlier e = Outliers {
                        samplesSeen = 1
                      , lowSevere = if e <= loS && e < hiM then 1 else 0
                      , lowMild = if e > loS && e <= loM then 1 else 0
                      , highMild = if e >= hiM && e < hiS then 1 else 0
                      , highSevere = if e >= hiS && e > loM then 1 else 0
                      }
          !loS = q1 - (iqr * 3)
          !loM = q1 - (iqr * 1.5)
          !hiM = q3 + (iqr * 1.5)
          !hiS = q3 + (iqr * 3)
          q1   = weightedAvg 1 4 (Sorted ssa)
          q3   = weightedAvg 3 4 (Sorted ssa)
          ssa  = sort sa
          iqr  = q3 - q1

-- | Compute the extent to which outliers in the sample data affect
-- the sample mean and standard deviation.
outlierVariance
  :: B.Estimate B.ConfInt Double -- ^ Bootstrap estimate of sample mean.
  -> B.Estimate B.ConfInt Double -- ^ Bootstrap estimate of sample
                                 --   standard deviation.
  -> Double                      -- ^ Number of original iterations.
  -> OutlierVariance
outlierVariance µ σ a = OutlierVariance effect desc varOutMin
  where
    ( effect, desc ) | varOutMin < 0.01 = (Unaffected, "no")
                     | varOutMin < 0.1  = (Slight,     "slight")
                     | varOutMin < 0.5  = (Moderate,   "moderate")
                     | otherwise        = (Severe,     "severe")
    varOutMin = (minBy varOut 1 (minBy cMax 0 µgMin)) / σb2
    varOut c  = (ac / a) * (σb2 - ac * σg2) where ac = a - c
    σb        = B.estPoint σ
    µa        = B.estPoint µ / a
    µgMin     = µa / 2
    σg        = min (µgMin / 4) (σb / sqrt a)
    σg2       = σg * σg
    σb2       = σb * σb
    minBy f q r = min (f q) (f r)
    cMax x    = fromIntegral (floor (-2 * k0 / (k1 + sqrt det)) :: Int)
      where
        k1    = σb2 - a * σg2 + ad
        k0    = -a * ad
        ad    = a * d
        d     = k * k where k = µa - x
        det   = k1 * k1 - 4 * σg2 * k0

-- | Count the total number of outliers in a sample.
countOutliers :: Outliers -> Int64
countOutliers (Outliers _ a b c d) = a + b + c + d
{-# INLINE countOutliers #-}

-- | Display the mean of a 'Sample', and characterise the outliers
-- present in the sample.
analyseMean :: Sample
            -> Int              -- ^ Number of iterations used to
                                -- compute the sample.
            -> Gauge Double
analyseMean a iters = do
  let µ = mean a
  _ <- note "mean is %s (%d iterations)\n" (secs µ) iters
  noteOutliers . classifyOutliers $ a
  return µ

-- | Multiply the 'Estimate's in an analysis by the given value, using
-- 'B.scale'.
scale :: Double                 -- ^ Value to multiply by.
      -> SampleAnalysis -> SampleAnalysis
scale f s@SampleAnalysis{..} = s {
                                 anMean = B.scale f anMean
                               , anStdDev = B.scale f anStdDev
                               }

-- | Perform an analysis of a measurement.
analyseSample :: String            -- ^ Experiment name.
              -> V.Vector Measured -- ^ Sample data.
              -> Gauge (Either String Report)
analyseSample name meas = do
  Config{..} <- askConfig
  overhead <- getOverhead
  let ests      = [Mean,StdDev]
      -- The use of filter here throws away very-low-quality
      -- measurements when bootstrapping the mean and standard
      -- deviations.  Without this, the numbers look nonsensical when
      -- very brief actions are measured.
      stime     = measure (measTime . rescale) .
                  G.filter ((>= threshold) . measTime) . G.map fixTime .
                  G.tail $ meas
      fixTime m = m { measTime = measTime m - overhead / 2 }
      n         = G.length meas
      s         = G.length stime
  _ <- prolix "bootstrapping with %d of %d samples (%d%%)\n" s n ((s * 100) `quot` n)

  gen <- getGen
  ers <- (sequence <$>) . mapM (\(ps,r) -> regress gen ps r meas) $ ((["iters"],"time"):regressions)
  case ers of
    Left err -> pure $ Left err
    Right rs -> do
      resamps <- gaugeIO $ resample gen ests resamples stime
      let [estMean,estStdDev] = B.bootstrapBCA confInterval stime resamps
          ov = outlierVariance estMean estStdDev (fromIntegral n)
          an = SampleAnalysis
                 { anRegress    = rs
                 , anOverhead   = overhead
                 , anMean       = estMean
                 , anStdDev     = estStdDev
                 , anOutlierVar = ov
                 }
      return $ Right $ Report
        { reportName     = name
        , reportKeys     = measureKeys
        , reportMeasured = meas
        , reportAnalysis = an
        , reportOutliers = classifyOutliers stime
        , reportKDEs     = [uncurry (KDE "time") (kde 128 stime)]
        }


-- | Regress the given predictors against the responder.
--
-- Errors may be returned under various circumstances, such as invalid
-- names or lack of needed data.
--
-- See 'olsRegress' for details of the regression performed.
regress :: GenIO
        -> [String]             -- ^ Predictor names.
        -> String               -- ^ Responder name.
        -> V.Vector Measured
        -> Gauge (Either String Regression)
regress gen predNames respName meas
    | G.null meas = pure $ Left "no measurements"
    | otherwise   = case validateAccessors predNames respName of
        Left err   -> pure $ Left err
        Right accs -> do
            let unmeasured = [n | (n, Nothing) <- map (second ($ G.head meas)) accs]
            if not (null unmeasured)
                then pure $ Left $ "no data available for " ++ renderNames unmeasured
                else do
                    let (r:ps) = map ((`measure` meas) . (fromJust .) . snd) accs
                    Config{..} <- askConfig
                    (coeffs,r2) <- gaugeIO $ bootstrapRegress gen resamples confInterval olsRegress ps r
                    pure $ Right $ Regression
                        { regResponder = respName
                        , regCoeffs    = Map.fromList (zip (predNames ++ ["y"]) (G.toList coeffs))
                        , regRSquare   = r2
                        }

singleton :: [a] -> Bool
singleton [_] = True
singleton _   = False

-- | Given a list of accessor names (see 'measureKeys'), return either
-- a mapping from accessor name to function or an error message if
-- any names are wrong.
resolveAccessors :: [String]
                 -> Either String [(String, Measured -> Maybe Double)]
resolveAccessors names =
  case unresolved of
    [] -> Right [(n, a) | (n, Just (a,_)) <- accessors]
    _  -> Left $ "unknown metric " ++ renderNames unresolved
  where
    unresolved = [n | (n, Nothing) <- accessors]
    accessors = flip map names $ \n -> (n, Map.lookup n measureAccessors)

-- | Given predictor and responder names, do some basic validation,
-- then hand back the relevant accessors.
validateAccessors :: [String]   -- ^ Predictor names.
                  -> String     -- ^ Responder name.
                  -> Either String [(String, Measured -> Maybe Double)]
validateAccessors predNames respName = do
  when (null predNames) $
    Left "no predictors specified"
  let names = respName:predNames
      dups = map head . filter (not . singleton) .
             List.group . List.sort $ names
  unless (null dups) $
    Left $ "duplicated metric " ++ renderNames dups
  resolveAccessors names

renderNames :: [String] -> String
renderNames = List.intercalate ", " . map show

-- | Display a report of the 'Outliers' present in a 'Sample'.
noteOutliers :: Outliers -> Gauge ()
noteOutliers o = do
  let frac n = (100::Double) * fromIntegral n / fromIntegral (samplesSeen o)
      check :: Int64 -> Double -> String -> Gauge ()
      check k t d = when (frac k > t) $
                    note "  %d (%.1g%%) %s\n" k (frac k) d
      outCount = countOutliers o
  when (outCount > 0) $ do
    _ <- note "found %d outliers among %d samples (%.1g%%)\n"
         outCount (samplesSeen o) (frac outCount)
    check (lowSevere o) 0 "low severe"
    check (lowMild o) 1 "low mild"
    check (highMild o) 1 "high mild"
    check (highSevere o) 0 "high severe"

-- | Analyse a single benchmark.
analyseBenchmark :: String -> V.Vector Measured -> Gauge Report
analyseBenchmark desc meas = do
  Config{..} <- askConfig
  _ <- prolix "analysing with %d resamples\n" resamples
  erp <- analyseSample desc meas
  case erp of
    Left err -> printError "*** Error: %s\n" err
    Right rpt@Report{..} -> do
        let SampleAnalysis{..} = reportAnalysis
            OutlierVariance{..} = anOutlierVar
            wibble = printOverallEffect ovEffect
            (builtin, others) = splitAt 1 anRegress
        case displayMode of
            StatsTable -> do
              _ <- note "%sbenchmarked %s\n" rewindClearLine desc
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

              reportStat Verbose measUtime (secs . (/ 1000000)) "User time"
              reportStat Verbose measStime (secs . (/ 1000000)) "System time"
              reportStat Verbose measMaxrss show "Maximum resident set size"
              reportStat Verbose measMinflt show "Minor page faults"
              reportStat Verbose measMajflt show "Major page faults"
              reportStat Verbose measNvcsw show "Total voluntary context switches"
              -- Always report involuntary context switches, may indicate too
              -- much load on the system when measuring.
              reportStat Quiet measNivcsw show "Total involuntary context switches"

              _ <- note "\n"
              pure ()
            Condensed -> do
              _ <- note "%s%-40s " rewindClearLine desc
              bsSmall secs "mean" anMean
              bsSmall secs "( +-" anStdDev
              _ <- note ")\n"
              pure ()

        return rpt
      where bs :: (Double -> String) -> String -> Estimate ConfInt Double -> Gauge ()
            bs f metric e@Estimate{..} =
              note "%-20s %-10s (%s .. %s%s)\n" metric
                   (f estPoint) (f $ fst $ confidenceInterval e) (f $ snd $ confidenceInterval e)
                   (let cl = confIntCL estError
                        str | cl == cl95 = ""
                            | otherwise  = printf ", ci %.3f" (confidenceLevel cl)
                    in str
                   )
            bsSmall :: (Double -> String) -> String -> Estimate ConfInt Double -> Gauge ()
            bsSmall f metric Estimate{..} =
              note "%s %-10s" metric (f estPoint)

            reportStat :: Verbosity -> (Measured -> Int64) -> (Double -> String)
                       -> String -> Gauge ()
            reportStat lvl accessor sh msg = do
              let v = V.map (accessor . rescale) meas
                  total = V.sum (V.map (accessor . rescale) meas)
                  len = V.length v
                  avg = (fromIntegral total) / (fromIntegral len)
              when (verbosity >= lvl && avg > 0.0) $ do
                note "%-20s %-10s (%s .. %s)\n" msg (sh avg)
                  (sh (fromIntegral (V.minimum v)))
                  (sh (fromIntegral (V.maximum v)))


printOverallEffect :: OutlierEffect -> String
printOverallEffect Unaffected = "unaffected"
printOverallEffect Slight     = "slightly inflated"
printOverallEffect Moderate   = "moderately inflated"
printOverallEffect Severe     = "severely inflated"
