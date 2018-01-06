{-# LANGUAGE CPP, BangPatterns, ScopedTypeVariables, ForeignFunctionInterface #-}
-- |
-- Module    : Numeric.SpecFunctions.Internal
-- Copyright : (c) 2009, 2011, 2012 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Internal module with implementation of special functions.
module Numeric.SpecFunctions.Internal
    ( erf
    , erfc
    , invErf
    , invErfc
    , log1p
    , log1pmx
    , log2
    , expm1
    ) where

#if !MIN_VERSION_base(4,9,0)
import Control.Applicative
#endif
import Data.Bits       ((.&.), (.|.), shiftR)
import Data.Word       (Word)
import qualified Data.Vector.Unboxed as U
#if MIN_VERSION_base(4,9,0)
import GHC.Float (log1p,expm1)
#else
import Numeric.Polynomial.Chebyshev    (chebyshevBroucke)
#endif

import Numeric.Series
import Numeric.MathFunctions.Constants

----------------------------------------------------------------
-- Error function
----------------------------------------------------------------

-- | Error function.
--
-- \[
-- \operatorname{erf}(x) = \frac{2}{\sqrt{\pi}} \int_{0}^{x} \exp(-t^2) dt
-- \]
--
-- Function limits are:
--
-- \[
-- \begin{aligned}
--  &\operatorname{erf}(-\infty) &=& -1 \\
--  &\operatorname{erf}(0)       &=& \phantom{-}\,0 \\
--  &\operatorname{erf}(+\infty) &=& \phantom{-}\,1 \\
-- \end{aligned}
-- \]
erf :: Double -> Double
{-# INLINE erf #-}
erf = c_erf

-- | Complementary error function.
--
-- \[
-- \operatorname{erfc}(x) = 1 - \operatorname{erf}(x)
-- \]
--
-- Function limits are:
--
-- \[
-- \begin{aligned}
--  &\operatorname{erf}(-\infty) &=&\, 2 \\
--  &\operatorname{erf}(0)       &=&\, 1 \\
--  &\operatorname{erf}(+\infty) &=&\, 0 \\
-- \end{aligned}
-- \]
erfc :: Double -> Double
{-# INLINE erfc #-}
erfc = c_erfc

foreign import ccall "erf"  c_erf  :: Double -> Double
foreign import ccall "erfc" c_erfc :: Double -> Double


-- | Inverse of 'erf'.
invErf :: Double -- ^ /p/ ∈ [-1,1]
       -> Double
invErf p = invErfc (1 - p)

-- | Inverse of 'erfc'.
invErfc :: Double -- ^ /p/ ∈ [0,2]
        -> Double
invErfc p
  | p == 2        = m_neg_inf
  | p == 0        = m_pos_inf
  | p >0 && p < 2 = if p <= 1 then r else -r
  | otherwise     = modErr $ "invErfc: p must be in [0,2] got " ++ show p
  where
    pp = if p <= 1 then p else 2 - p
    t  = sqrt $ -2 * log( 0.5 * pp)
    -- Initial guess
    x0 = -0.70711 * ((2.30753 + t * 0.27061) / (1 + t * (0.99229 + t * 0.04481)) - t)
    r  = loop 0 x0
    --
    loop :: Int -> Double -> Double
    loop !j !x
      | j >= 2    = x
      | otherwise = let err = erfc x - pp
                        x'  = x + err / (1.12837916709551257 * exp(-x * x) - x * err) -- // Halley
                    in loop (j+1) x'




----------------------------------------------------------------
-- Logarithm
----------------------------------------------------------------

-- GHC.Float provides log1p and expm1 since 4.9.0
#if !MIN_VERSION_base(4,9,0)
-- | Compute the natural logarithm of 1 + @x@.  This is accurate even
-- for values of @x@ near zero, where use of @log(1+x)@ would lose
-- precision.
log1p :: Double -> Double
log1p x
    | x == 0               = 0
    | x == -1              = m_neg_inf
    | x < -1               = m_NaN
    | x' < m_epsilon * 0.5 = x
    | (x >= 0 && x < 1e-8) || (x >= -1e-9 && x < 0)
                           = x * (1 - x * 0.5)
    | x' < 0.375           = x * (1 - x * chebyshevBroucke (x / 0.375) coeffs)
    | otherwise            = log (1 + x)
  where
    x' = abs x
    coeffs = U.fromList [
               0.10378693562743769800686267719098e+1,
              -0.13364301504908918098766041553133e+0,
               0.19408249135520563357926199374750e-1,
              -0.30107551127535777690376537776592e-2,
               0.48694614797154850090456366509137e-3,
              -0.81054881893175356066809943008622e-4,
               0.13778847799559524782938251496059e-4,
              -0.23802210894358970251369992914935e-5,
               0.41640416213865183476391859901989e-6,
              -0.73595828378075994984266837031998e-7,
               0.13117611876241674949152294345011e-7,
              -0.23546709317742425136696092330175e-8,
               0.42522773276034997775638052962567e-9,
              -0.77190894134840796826108107493300e-10,
               0.14075746481359069909215356472191e-10,
              -0.25769072058024680627537078627584e-11,
               0.47342406666294421849154395005938e-12,
              -0.87249012674742641745301263292675e-13,
               0.16124614902740551465739833119115e-13,
              -0.29875652015665773006710792416815e-14,
               0.55480701209082887983041321697279e-15,
              -0.10324619158271569595141333961932e-15
             ]

-- | Compute @exp x - 1@ without loss of accuracy for x near zero.
expm1 :: Double -> Double
#ifdef USE_SYSTEM_EXPM1
expm1 = c_expm1

foreign import ccall "expm1" c_expm1 :: Double -> Double
#else
-- NOTE: this is simplest implementation and not terribly efficient.
expm1 x
  | x < (-37.42994775023705) = -1
  | x > m_max_log            = m_pos_inf
  | abs x > 0.5              = exp x - 1
  | otherwise                = sumSeries $ (*) <$> (scanSequence (*) x (pure x))
                                               <*> (1 / scanSequence (*) 1 (enumSequenceFrom 2))
#endif
#endif

-- | Compute log(1+x)-x:
log1pmx :: Double -> Double
log1pmx x
  | x <  -1        = error "Domain error"
  | x == -1        = m_neg_inf
  | ax > 0.95      = log(1 + x) - x
  | ax < m_epsilon = -(x * x) /2
  | otherwise      = - x * x * sumPowerSeries (-x) (recip <$> enumSequenceFrom 2)
  where
   ax = abs x

-- | /O(log n)/ Compute the logarithm in base 2 of the given value.
log2 :: Int -> Int
log2 v0
    | v0 <= 0   = modErr $ "log2: nonpositive input, got " ++ show v0
    | otherwise = go 5 0 v0
  where
    go !i !r !v | i == -1        = r
                | v .&. b i /= 0 = let si = U.unsafeIndex sv i
                                   in go (i-1) (r .|. si) (v `shiftR` si)
                | otherwise      = go (i-1) r v
    b = U.unsafeIndex bv
    !bv = U.fromList [ 0x02, 0x0c, 0xf0, 0xff00
                     , fromIntegral (0xffff0000 :: Word)
                     , fromIntegral (0xffffffff00000000 :: Word)]
    !sv = U.fromList [1,2,4,8,16,32]

modErr :: String -> a
modErr msg = error $ "Numeric.SpecFunctions." ++ msg
