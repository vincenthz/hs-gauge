-- |
-- Module    : Numeric.MathFunctions.Comparison
-- Copyright : (c) 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Functions for approximate comparison of floating point numbers.
--
-- Approximate floating point comparison, based on Bruce Dawson's
-- \"Comparing floating point numbers\":
-- <http://www.cygnus-software.com/papers/comparingfloats/comparingfloats.htm>
module Numeric.MathFunctions.Comparison
    ( -- * Relative erros
      relativeError
    , eqRelErr
      -- * Ulps-based comparison
    , addUlps
    , ulpDistance
    , ulpDelta
    , within
    ) where

import Basement.Floating (doubleToWord, wordToDouble)
import Data.Word (Word64)
import Data.Int (Int64)


----------------------------------------------------------------
-- Ulps-based comparison
----------------------------------------------------------------

-- |
-- Calculate relative error of two numbers:
--
-- \[ \frac{|a - b|}{\max(|a|,|b|)} \]
--
-- It lies in [0,1) interval for numbers with same sign and (1,2] for
-- numbers with different sign. If both arguments are zero or negative
-- zero function returns 0. If at least one argument is transfinite it
-- returns NaN
relativeError :: Double -> Double -> Double
relativeError a b
  | a == 0 && b == 0 = 0
  | otherwise        = abs (a - b) / max (abs a) (abs b)

-- | Check that relative error between two numbers @a@ and @b@. If
-- 'relativeError' returns NaN it returns @False@.
eqRelErr :: Double -- ^ /eps/ relative error should be in [0,1) range
         -> Double -- ^ /a/
         -> Double -- ^ /b/
         -> Bool
eqRelErr eps a b = relativeError a b < eps



----------------------------------------------------------------
-- Ulps-based comparison
----------------------------------------------------------------

-- |
-- Add N ULPs (units of least precision) to @Double@ number.
addUlps :: Int -> Double -> Double
addUlps n a =
  -- Convert to ulps number represented as Int64
  let big     = 0x8000000000000000
      order :: Word64 -> Int64
      order i | i < big   = fromIntegral i
              | otherwise = fromIntegral $ maxBound - (i - big)
      unorder :: Int64 -> Word64
      unorder i | i >= 0    = fromIntegral i
                | otherwise = big + (maxBound - (fromIntegral i))
      ai0' = unorder $ order ai0 + fromIntegral n
   in wordToDouble ai0'
  where
    ai0 = doubleToWord a

-- |
-- Measure distance between two @Double@s in ULPs (units of least
-- precision). Note that it's different from @abs (ulpDelta a b)@
-- since it returns correct result even when 'ulpDelta' overflows.
ulpDistance :: Double
            -> Double
            -> Word64
ulpDistance a b =
  -- IEEE754 floats use most significant bit as sign bit (not
  -- 2-complement) and we need to rearrange representations of float
  -- number so that they could be compared lexicographically as
  -- Word64.
  let big     = 0x8000000000000000
      order i | i < big   = i + big
              | otherwise = maxBound - i
      ai = order ai0
      bi = order bi0
      d  | ai > bi   = ai - bi
         | otherwise = bi - ai
   in d
  where
    ai0 = doubleToWord a
    bi0 = doubleToWord b

-- |
-- Measure signed distance between two @Double@s in ULPs (units of least
-- precision). Note that unlike 'ulpDistance' it can overflow.
--
-- > >>> ulpDelta 1 (1 + m_epsilon)
-- > 1
ulpDelta :: Double
         -> Double
         -> Int64
ulpDelta a b =
  -- IEEE754 floats use most significant bit as sign bit (not
  -- 2-complement) and we need to rearrange representations of float
  -- number so that they could be compared lexicographically as
  -- Word64.
  let big     = 0x8000000000000000 :: Word64
      order i | i < big   = i + big
              | otherwise = maxBound - i
      ai = order ai0
      bi = order bi0
   in fromIntegral $ bi - ai
  where
    ai0 = doubleToWord a
    bi0 = doubleToWord b


-- | Compare two 'Double' values for approximate equality, using
-- Dawson's method.
--
-- The required accuracy is specified in ULPs (units of least
-- precision).  If the two numbers differ by the given number of ULPs
-- or less, this function returns @True@.
within :: Int                   -- ^ Number of ULPs of accuracy desired.
       -> Double -> Double -> Bool
within ulps a b
  | ulps < 0  = False
  | otherwise = ulpDistance a b <= fromIntegral ulps
