-- |
-- Module    : Numeric.SpecFunctions.Extra
-- Copyright : (c) 2009, 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Less common mathematical functions.
module Numeric.SpecFunctions.Extra (
    bd0
  , chooseExact
  , logChooseFast
  , logGammaAS245
  , logGammaCorrection
  ) where

import Numeric.MathFunctions.Constants (m_NaN,m_pos_inf)
import Numeric.SpecFunctions.Internal  (chooseExact,logChooseFast,logGammaCorrection)

-- | Evaluate the deviance term @x log(x/np) + np - x@.
bd0 :: Double                   -- ^ @x@
    -> Double                   -- ^ @np@
    -> Double 
bd0 x np 
  | isInfinite x || isInfinite np || np == 0 = m_NaN
  | abs x_np >= 0.1*(x+np)                   = x * log (x/np) - x_np
  | otherwise                                = loop 1 (ej0*vv) s0
  where 
    x_np = x - np
    v    = x_np / (x+np)
    s0   = x_np * v
    ej0  = 2*x*v
    vv   = v*v
    loop j ej s = case s + ej/(2*j+1) of
                    s' | s' == s   -> s'  -- FIXME: Comparing Doubles for equality!
                       | otherwise -> loop (j+1) (ej*vv) s'



-- | Compute the logarithm of the gamma function Γ(/x/).  Uses
-- Algorithm AS 245 by Macleod.
--
-- Gives an accuracy of 10-12 significant decimal digits, except
-- for small regions around /x/ = 1 and /x/ = 2, where the function
-- goes to zero.  For greater accuracy, use 'logGammaL'.
--
-- Returns ∞ if the input is outside of the range (0 < /x/ ≤ 1e305).
logGammaAS245 :: Double -> Double
-- Adapted from http://people.sc.fsu.edu/~burkardt/f_src/asa245/asa245.html
logGammaAS245 x
    | x <= 0    = m_pos_inf
    -- Handle positive infinity. logGamma overflows before 1e308 so
    -- it's safe
    | x > 1e308 = m_pos_inf
    -- Normal cases
    | x < 1.5   = a + c *
                  ((((r1_4 * b + r1_3) * b + r1_2) * b + r1_1) * b + r1_0) /
                  ((((b + r1_8) * b + r1_7) * b + r1_6) * b + r1_5)
    | x < 4     = (x - 2) *
                  ((((r2_4 * x + r2_3) * x + r2_2) * x + r2_1) * x + r2_0) /
                  ((((x + r2_8) * x + r2_7) * x + r2_6) * x + r2_5)
    | x < 12    = ((((r3_4 * x + r3_3) * x + r3_2) * x + r3_1) * x + r3_0) /
                  ((((x + r3_8) * x + r3_7) * x + r3_6) * x + r3_5)
    | x > 3e6   = k
    | otherwise = k + x1 *
                  ((r4_2 * x2 + r4_1) * x2 + r4_0) /
                  ((x2 + r4_4) * x2 + r4_3)
  where
    (a , b , c)
        | x < 0.5   = (-y , x + 1 , x)
        | otherwise = (0  , x     , x - 1)

    y      = log x
    k      = x * (y-1) - 0.5 * y + alr2pi
    alr2pi = 0.918938533204673

    x1 = 1 / x
    x2 = x1 * x1

    r1_0 =  -2.66685511495;   r1_1 =  -24.4387534237;    r1_2 = -21.9698958928
    r1_3 =  11.1667541262;    r1_4 =    3.13060547623;   r1_5 =   0.607771387771
    r1_6 =  11.9400905721;    r1_7 =   31.4690115749;    r1_8 =  15.2346874070

    r2_0 = -78.3359299449;    r2_1 = -142.046296688;     r2_2 = 137.519416416
    r2_3 =  78.6994924154;    r2_4 =    4.16438922228;   r2_5 =  47.0668766060
    r2_6 = 313.399215894;     r2_7 =  263.505074721;     r2_8 =  43.3400022514

    r3_0 =  -2.12159572323e5; r3_1 =    2.30661510616e5; r3_2 =   2.74647644705e4
    r3_3 =  -4.02621119975e4; r3_4 =   -2.29660729780e3; r3_5 =  -1.16328495004e5
    r3_6 =  -1.46025937511e5; r3_7 =   -2.42357409629e4; r3_8 =  -5.70691009324e2

    r4_0 = 0.279195317918525;  r4_1 = 0.4917317610505968;
    r4_2 = 0.0692910599291889; r4_3 = 3.350343815022304
    r4_4 = 6.012459259764103
