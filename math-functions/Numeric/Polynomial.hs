-- |
-- Module    : Numeric.Polynomial
-- Copyright : (c) 2012 Aleksey Khudyakov
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Function for evaluating polynomials using Horher's method.
module Numeric.Polynomial (
    -- * Polynomials
    evaluatePolynomial
  , evaluateEvenPolynomial
  , evaluateOddPolynomial
    -- ** Lists
    -- $list
  , evaluatePolynomialL
  , evaluateEvenPolynomialL
  , evaluateOddPolynomialL
  ) where

import qualified Data.Vector.Generic as G
import qualified Data.Vector         as V
import           Data.Vector.Generic  (Vector)


-- | Evaluate polynomial using Horner's method. Coefficients starts
-- from lowest. In pseudocode:
--
-- > evaluateOddPolynomial x [1,2,3] = 1 + 2*x + 3*x^2
evaluatePolynomial :: (Vector v a, Num a)
                   => a    -- ^ /x/
                   -> v a  -- ^ Coefficients
                   -> a
{-# INLINE evaluatePolynomial #-}
evaluatePolynomial x v
  | G.null v  = 0
  | otherwise = G.foldr1 (\a r -> a + r*x) v

-- | Evaluate polynomial with only even powers using Horner's method.
-- Coefficients starts from lowest. In pseudocode:
--
-- > evaluateOddPolynomial x [1,2,3] = 1 + 2*x^2 + 3*x^4
evaluateEvenPolynomial :: (Vector v a, Num a)
                       => a    -- ^ /x/
                       -> v a  -- ^ Coefficients
                       -> a
{-# INLINE evaluateEvenPolynomial #-}
evaluateEvenPolynomial x
  = evaluatePolynomial (x*x)


-- | Evaluate polynomial with only odd powers using Horner's method.
-- Coefficients starts from lowest. In pseudocode:
--
-- > evaluateOddPolynomial x [1,2,3] = 1*x + 2*x^3 + 3*x^5
evaluateOddPolynomial :: (Vector v a, Num a)
                       => a    -- ^ /x/
                       -> v a  -- ^ Coefficients
                       -> a
{-# INLINE evaluateOddPolynomial #-}
evaluateOddPolynomial x coefs
  = x * evaluatePolynomial (x*x) coefs




-- $lists
--
-- When all coefficients are known statically it's more convenient to
-- pass coefficient in a list instad of vector. Functions below
-- provide just that functionality. If list is known statically it
-- will be inlined anyway.

evaluatePolynomialL :: (Num a) => a -> [a] -> a
evaluatePolynomialL x = evaluatePolynomial x . V.fromList
{-# INLINE evaluatePolynomialL #-}

evaluateEvenPolynomialL :: (Num a) => a -> [a] -> a
evaluateEvenPolynomialL x = evaluateEvenPolynomial x . V.fromList
{-# INLINE evaluateEvenPolynomialL #-}

evaluateOddPolynomialL :: (Num a) => a -> [a] -> a
evaluateOddPolynomialL x = evaluateOddPolynomial x . V.fromList
{-# INLINE evaluateOddPolynomialL #-}
