{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
-- |
-- Module    : Numeric.Series
-- Copyright : (c) 2016 Alexey Khudyakov
-- License   : BSD3
--
-- Maintainer  : alexey.skladnoy@gmail.com, bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Functions for working with infinite sequences. In particular
-- summation of series and evaluation of continued fractions.
module Numeric.Series (
    -- * Data type for infinite sequences.
    Sequence(..)
    -- * Constructors
  , enumSequenceFrom
  , enumSequenceFromStep
  , scanSequence
    -- * Summation of series
  , sumSeries
  , sumPowerSeries
  , sequenceToList
    -- * Evaluation of continued fractions
  , evalContFractionB
  ) where

import Control.Applicative
import Data.List (unfoldr)

import Numeric.MathFunctions.Constants (m_epsilon)


----------------------------------------------------------------

-- | Infinite series. It's represented as opaque state and step
--   function.
data Sequence a = forall s. Sequence s (s -> (a,s))

instance Functor Sequence where
  fmap f (Sequence s0 step) = Sequence s0 (\s -> let (a,s') = step s in (f a, s'))
  {-# INLINE fmap #-}

instance Applicative Sequence where
  pure a = Sequence () (\() -> (a,()))
  Sequence sA fA <*> Sequence sB fB = Sequence (sA,sB) $ \(!sa,!sb) ->
    let (a,sa') = fA sa
        (b,sb') = fB sb
    in (a b, (sa',sb'))
  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}

-- | Elementwise operations with sequences
instance Num a => Num (Sequence a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  {-# INLINE (+) #-}
  {-# INLINE (*) #-}
  {-# INLINE (-) #-}
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = pure . fromInteger
  {-# INLINE abs         #-}
  {-# INLINE signum      #-}
  {-# INLINE fromInteger #-}

-- | Elementwise operations with sequences
instance Fractional a => Fractional (Sequence a) where
  (/)          = liftA2 (/)
  recip        = fmap recip
  fromRational = pure . fromRational
  {-# INLINE (/)          #-}
  {-# INLINE recip        #-}
  {-# INLINE fromRational #-}



----------------------------------------------------------------
-- Constructors
----------------------------------------------------------------

-- | @enumSequenceFrom x@ generate sequence:
--
-- \[ a_n = x + n \]
enumSequenceFrom :: Num a => a -> Sequence a
enumSequenceFrom i = Sequence i (\n -> (n,n+1))
{-# INLINE enumSequenceFrom #-}

-- | @enumSequenceFromStep x d@ generate sequence:
--
-- \[ a_n = x + nd \]
enumSequenceFromStep :: Num a => a -> a -> Sequence a
enumSequenceFromStep n d = Sequence n (\i -> (i,i+d))
{-# INLINE enumSequenceFromStep #-}

-- | Analog of 'scanl' for sequence.
scanSequence :: (b -> a -> b) -> b -> Sequence a -> Sequence b
{-# INLINE scanSequence #-}
scanSequence f b0 (Sequence s0 step) = Sequence (b0,s0) $ \(b,s) ->
  let (a,s') = step s
      b'     = f b a
  in (b,(b',s'))


----------------------------------------------------------------
-- Evaluation of series
----------------------------------------------------------------

-- | Calculate sum of series
--
-- \[ \sum_{i=0}^\infty a_i \]
--
-- Summation is stopped when
--
-- \[ a_{n+1} < \varepsilon\sum_{i=0}^n a_i \]
--
-- where ε is machine precision ('m_epsilon')
sumSeries :: Sequence Double -> Double
{-# INLINE sumSeries #-}
sumSeries (Sequence sInit step)
  = go x0 s0
  where 
    (x0,s0) = step sInit
    go x s | abs (d/x) >= m_epsilon = go x' s'
           | otherwise              = x'
      where (d,s') = step s
            x'     = x + d

-- | Calculate sum of series
--
-- \[ \sum_{i=0}^\infty x^ia_i \]
--
-- Calculation is stopped when next value in series is less than
-- ε·sum.
sumPowerSeries :: Double -> Sequence Double -> Double
sumPowerSeries x ser = sumSeries $ liftA2 (*) (scanSequence (*) 1 (pure x)) ser
{-# INLINE sumPowerSeries #-}

-- | Convert series to infinite list
sequenceToList :: Sequence a -> [a]
sequenceToList (Sequence s f) = unfoldr (Just . f) s



----------------------------------------------------------------
-- Evaluation of continued fractions
----------------------------------------------------------------

-- |
-- Evaluate continued fraction using modified Lentz algorithm.
-- Sequence contain pairs (a[i],b[i]) which form following expression:
--
-- \[
-- b_0 + \frac{a_1}{b_1+\frac{a_2}{b_2+\frac{a_3}{b_3 + \cdots}}}
-- \]
--
-- Modified Lentz algorithm is described in Numerical recipes 5.2
-- "Evaluation of Continued Fractions"
evalContFractionB :: Sequence (Double,Double) -> Double
{-# INLINE evalContFractionB #-}
evalContFractionB (Sequence sInit step)
  = let ((_,b0),s0) = step sInit
        f0          = maskZero b0
    in  go f0 f0 0 s0
  where
    tiny = 1e-60
    maskZero 0 = tiny
    maskZero x = x
    
    go f c d s
      | abs (delta - 1) >= m_epsilon = go f' c' d' s'
      | otherwise                    = f'
      where
          ((a,b),s') = step s
          d'    = recip $ maskZero $ b + a*d
          c'    = maskZero $ b + a/c 
          delta = c'*d'
          f'    = f*delta
