{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module    : Numeric.Polynomial.Chebyshev
-- Copyright : (c) 2009, 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Chebyshev polynomials.
module Numeric.Polynomial.Chebyshev (
    -- * Chebyshev polinomials
    -- $chebyshev
    chebyshev
  , chebyshevBroucke
    -- * References
    -- $references
  ) where

import qualified Data.Vector.Generic as G



-- $chebyshev
--
-- A Chebyshev polynomial of the first kind is defined by the
-- following recurrence:
--
-- \[\begin{aligned}
-- T_0(x)     &= 1 \\
-- T_1(x)     &= x \\
-- T_{n+1}(x) &= 2xT_n(x) - T_{n-1}(x) \\
-- \end{aligned}
-- \]

data C = C {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- | Evaluate a Chebyshev polynomial of the first kind. Uses
-- Clenshaw's algorithm.
chebyshev :: (G.Vector v Double) =>
             Double      -- ^ Parameter of each function.
          -> v Double    -- ^ Coefficients of each polynomial term, in increasing order.
          -> Double
chebyshev x a = fini . G.foldr' step (C 0 0) . G.tail $ a
    where step k (C b0 b1) = C (k + x2 * b0 - b1) b0
          fini   (C b0 b1) = G.head a + x * b0 - b1
          x2               = x * 2
{-# INLINE chebyshev #-}

data B = B {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- | Evaluate a Chebyshev polynomial of the first kind. Uses Broucke's
-- ECHEB algorithm, and his convention for coefficient handling. It
-- treat 0th coefficient different so
--
-- > chebyshev x [a0,a1,a2...] == chebyshevBroucke [2*a0,a1,a2...]
chebyshevBroucke :: (G.Vector v Double) =>
             Double      -- ^ Parameter of each function.
          -> v Double    -- ^ Coefficients of each polynomial term, in increasing order.
          -> Double
chebyshevBroucke x = fini . G.foldr' step (B 0 0 0)
    where step k (B b0 b1 _) = B (k + x2 * b0 - b1) b0 b1
          fini   (B b0 _ b2) = (b0 - b2) * 0.5
          x2                 = x * 2
{-# INLINE chebyshevBroucke #-}



-- $references
--
-- * Broucke, R. (1973) Algorithm 446: Ten subroutines for the
--   manipulation of Chebyshev series. /Communications of the ACM/
--   16(4):254&#8211;256.  <http://doi.acm.org/10.1145/362003.362037>
--
-- * Clenshaw, C.W. (1962) Chebyshev series for mathematical
--   functions. /National Physical Laboratory Mathematical Tables 5/,
--   Her Majesty's Stationery Office, London.
--
