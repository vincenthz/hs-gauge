{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleContexts,
    MagicHash, Rank2Types, ScopedTypeVariables, TypeFamilies, UnboxedTuples,
    ForeignFunctionInterface #-}
-- |
-- Module    : System.Random.MWC
-- Copyright : (c) 2009-2012 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Pseudo-random number generation.  This module contains code for
-- generating high quality random numbers that follow a uniform
-- distribution.
--
-- For non-uniform distributions, see the
-- 'System.Random.MWC.Distributions' module.
--
-- The uniform PRNG uses Marsaglia's MWC256 (also known as MWC8222)
-- multiply-with-carry generator, which has a period of 2^8222 and
-- fares well in tests of randomness.  It is also extremely fast,
-- between 2 and 3 times faster than the Mersenne Twister.
--
-- The generator state is stored in the 'Gen' data type. It can be
-- created in several ways:
--
--   1. Using the 'withSystemRandom' call, which creates a random state.
--
--   2. Supply your own seed to 'initialize' function.
--
--   3. Finally, 'create' makes a generator from a fixed seed.
--      Generators created in this way aren't really random.
--
-- For repeatability, the state of the generator can be snapshotted
-- and replayed using the 'save' and 'restore' functions.
--
-- The simplest use is to generate a vector of uniformly distributed values:
--
-- @
--   vs \<- 'withSystemRandom' . 'asGenST' $ \\gen -> 'uniformVector' gen 100
-- @
--
-- These values can be of any type which is an instance of the class
-- 'Variate'.
--
-- To generate random values on demand, first 'create' a random number
-- generator.
--
-- @
--   gen <- 'create'
-- @
--
-- Hold onto this generator and use it wherever random values are
-- required (creating a new generator is expensive compared to
-- generating a random number, so you don't want to throw them
-- away). Get a random value using 'uniform' or 'uniformR':
--
-- @
--   v <- 'uniform' gen
-- @
--
-- @
--   v <- 'uniformR' (1, 52) gen
-- @
module System.Random.MWC
    (
    -- * Gen: Pseudo-Random Number Generators
      Gen
    , initialize
    , createSystemRandom
    , GenIO
    , splitGen

    -- * Variates: uniformly distributed values
    , Variate(..)
    , uniformVector

    ) where

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

import Control.Monad           (liftM, replicateM)
import Data.Bits               ((.&.), (.|.), shiftL, shiftR, xor)
import Data.Int                (Int8, Int16, Int32, Int64)
import Data.Vector.Generic     (Vector)
import Data.Word               (Word8, Word16, Word32, Word64)
#if !MIN_VERSION_base(4,8,0)
import Data.Word               (Word)
#endif
import Foreign.Marshal.Alloc   (allocaBytes)
import Foreign.Marshal.Array   (peekArray)
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Unboxed         as I
import qualified Data.Vector.Unboxed.Mutable as M
import System.IO        (IOMode(..), hGetBuf, withBinaryFile)
#if defined(mingw32_HOST_OS)
import Foreign.Ptr
import Foreign.C.Types
#endif

import Basement.Monad (PrimMonad(..))


-- | The class of types for which we can generate uniformly
-- distributed random variates.
--
-- The uniform PRNG uses Marsaglia's MWC256 (also known as MWC8222)
-- multiply-with-carry generator, which has a period of 2^8222 and
-- fares well in tests of randomness.  It is also extremely fast,
-- between 2 and 3 times faster than the Mersenne Twister.
--
-- /Note/: Marsaglia's PRNG is not known to be cryptographically
-- secure, so you should not use it for cryptographic operations.
class Variate a where
    -- | Generate a single uniformly distributed random variate.  The
    -- range of values produced varies by type:
    --
    -- * For fixed-width integral types, the type's entire range is
    --   used.
    --
    -- * For floating point numbers, the range (0,1] is used. Zero is
    --   explicitly excluded, to allow variates to be used in
    --   statistical calculations that require non-zero values
    --   (e.g. uses of the 'log' function).
    --
    -- To generate a 'Float' variate with a range of [0,1), subtract
    -- 2**(-33).  To do the same with 'Double' variates, subtract
    -- 2**(-53).
    uniform :: Gen -> IO a
    -- | Generate single uniformly distributed random variable in a
    -- given range.
    --
    -- * For integral types inclusive range is used.
    --
    -- * For floating point numbers range (a,b] is used if one ignores
    --   rounding errors.
    uniformR :: (a,a) -> Gen -> IO a

instance Variate Word32 where
    uniform  = uniform1 fromIntegral
    uniformR a b = uniformRange a b
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}

instance Variate Word64 where
    uniform  = uniform2 wordsTo64Bit
    uniformR a b = uniformRange a b
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}

instance Variate Int where
#if WORD_SIZE_IN_BITS < 64
    uniform = uniform1 fromIntegral
#else
    uniform = uniform2 wordsTo64Bit
#endif
    uniformR a b = uniformRange a b
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}

instance Variate Word where
#if WORD_SIZE_IN_BITS < 64
    uniform = uniform1 fromIntegral
#else
    uniform = uniform2 wordsTo64Bit
#endif
    uniformR a b = uniformRange a b
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}

{-
instance (Variate a, Variate b) => Variate (a,b) where
    uniform g = (,) `liftM` uniform g `ap` uniform g
    uniformR ((x1,y1),(x2,y2)) g = (,) `liftM` uniformR (x1,x2) g `ap` uniformR (y1,y2) g
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}

instance (Variate a, Variate b, Variate c) => Variate (a,b,c) where
    uniform g = (,,) `liftM` uniform g `ap` uniform g `ap` uniform g
    uniformR ((x1,y1,z1),(x2,y2,z2)) g =
      (,,) `liftM` uniformR (x1,x2) g `ap` uniformR (y1,y2) g `ap` uniformR (z1,z2) g
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}

instance (Variate a, Variate b, Variate c, Variate d) => Variate (a,b,c,d) where
    uniform g = (,,,) `liftM` uniform g `ap` uniform g `ap` uniform g
                `ap` uniform g
    uniformR ((x1,y1,z1,t1),(x2,y2,z2,t2)) g =
      (,,,) `liftM` uniformR (x1,x2) g `ap` uniformR (y1,y2) g `ap`
                    uniformR (z1,z2) g `ap` uniformR (t1,t2) g
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}
-}

wordsTo64Bit :: (Integral a) => Word32 -> Word32 -> a
wordsTo64Bit x y =
    fromIntegral ((fromIntegral x `shiftL` 32) .|. fromIntegral y :: Word64)
{-# INLINE wordsTo64Bit #-}

-- | State of the pseudo-random number generator. It uses mutable
-- state so same generator shouldn't be used from the different
-- threads simultaneously.
newtype Gen = Gen (M.MVector (PrimState IO) Word32)

-- | A shorter name for PRNG state in the 'IO' monad.
type GenIO = Gen

ioff, coff :: Int
ioff = 256
coff = 257

-- | Create a generator for variates using the given seed, of which up
-- to 256 elements will be used.  For arrays of less than 256
-- elements, part of the default seed will be used to finish
-- initializing the generator's state.
--
-- Examples:
--
-- > initialize (singleton 42)
--
-- > initialize (fromList [4, 8, 15, 16, 23, 42])
--
-- If a seed contains fewer than 256 elements, it is first used
-- verbatim, then its elements are 'xor'ed against elements of the
-- default seed until 256 elements are reached.
--
-- If a seed contains exactly 258 elements, then the last two elements
-- are used to set the generator's initial state. This allows for
-- complete generator reproducibility, so that e.g. @gen' == gen@ in
-- the following example:
--
-- @gen' <- 'initialize' . 'fromSeed' =<< 'save'@
initialize :: I.Vector Word32 -> IO Gen
initialize seed = do
    q <- M.unsafeNew 258
    fill q
    if fini == 258
      then do
        M.unsafeWrite q ioff $ G.unsafeIndex seed ioff .&. 255
        M.unsafeWrite q coff $ G.unsafeIndex seed coff
      else do
        M.unsafeWrite q ioff 255
        M.unsafeWrite q coff 362436
    return (Gen q)
  where fill q = go 0 where
          go i | i == 256  = return ()
               | otherwise = M.unsafeWrite q i s >> go (i+1)
            where s | i >= fini = if fini == 0
                                  then G.unsafeIndex defaultSeed i
                                  else G.unsafeIndex defaultSeed i `xor`
                                       G.unsafeIndex seed (i `mod` fini)
                    | otherwise = G.unsafeIndex seed i
        fini = G.length seed
{-# INLINE initialize #-}

-- | Acquire seed from the system entropy source. On Unix machines,
-- this will attempt to use @/dev/urandom@. On Windows, it will internally
-- use @RtlGenRandom@.
acquireSeedSystem :: IO [Word32]
acquireSeedSystem = do
#if !defined(mingw32_HOST_OS)
  -- Read 256 random Word32s from /dev/urandom
  let nbytes = 1024
      random = "/dev/urandom"
  allocaBytes nbytes $ \buf -> do
    nread <- withBinaryFile random ReadMode $
               \h -> hGetBuf h buf nbytes
    peekArray (nread `div` 4) buf
#else
  let nbytes = 1024
  -- Generate 256 random Word32s from RtlGenRandom
  allocaBytes nbytes $ \buf -> do
    ok <- c_RtlGenRandom buf (fromIntegral nbytes)
    if ok then return () else fail "Couldn't use RtlGenRandom"
    peekArray (nbytes `div` 4) buf

-- Note: on 64-bit Windows, the 'stdcall' calling convention
-- isn't supported, so we use 'ccall' instead.
#if defined(i386_HOST_ARCH)
# define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
# define WINDOWS_CCONV ccall
#else
# error Unknown mingw32 architecture!
#endif

-- Note: On Windows, the typical convention would be to use
-- the CryptoGenRandom API in order to generate random data.
-- However, here we use 'SystemFunction036', AKA RtlGenRandom.
--
-- This is a commonly used API for this purpose; one bonus is
-- that it avoids having to bring in the CryptoAPI library,
-- and completely sidesteps the initialization cost of CryptoAPI.
--
-- While this function is technically "subject to change" that is
-- extremely unlikely in practice: rand_s in the Microsoft CRT uses
-- this, and they can't change it easily without also breaking
-- backwards compatibility with e.g. statically linked applications.
--
-- The name 'SystemFunction036' is the actual link-time name; the
-- display name is just for giggles, I guess.
--
-- See also:
--   - http://blogs.msdn.com/b/michael_howard/archive/2005/01/14/353379.aspx
--   - https://bugzilla.mozilla.org/show_bug.cgi?id=504270
--
foreign import WINDOWS_CCONV unsafe "SystemFunction036"
  c_RtlGenRandom :: Ptr a -> CULong -> IO Bool
#endif

-- | Seed a PRNG with data from the system's fast source of pseudo-random
-- numbers.
createSystemRandom :: IO GenIO
createSystemRandom = do
  seed <- acquireSeedSystem
  initialize (I.fromList seed)

-- | Compute the next index into the state pool.  This is simply
-- addition modulo 256.
nextIndex :: Integral a => a -> Int
nextIndex i = fromIntegral j
    where j = fromIntegral (i+1) :: Word8
{-# INLINE nextIndex #-}

aa :: Word64
aa = 1540315826
{-# INLINE aa #-}

uniformWord32 :: Gen -> IO Word32
uniformWord32 (Gen q) = do
  i  <- nextIndex `liftM` M.unsafeRead q ioff
  c  <- fromIntegral `liftM` M.unsafeRead q coff
  qi <- fromIntegral `liftM` M.unsafeRead q i
  let t  = aa * qi + c
      c' = fromIntegral (t `shiftR` 32)
      x  = fromIntegral t + c'
      (# x', c'' #)  | x < c'    = (# x + 1, c' + 1 #)
                     | otherwise = (# x,     c' #)
  M.unsafeWrite q i x'
  M.unsafeWrite q ioff (fromIntegral i)
  M.unsafeWrite q coff (fromIntegral c'')
  return x'
{-# INLINE uniformWord32 #-}

uniform1 :: (Word32 -> a) -> Gen -> IO a
uniform1 f gen = do
  i <- uniformWord32 gen
  return $! f i
{-# INLINE uniform1 #-}

uniform2 :: (Word32 -> Word32 -> a) -> Gen -> IO a
uniform2 f (Gen q) = do
  i  <- nextIndex `liftM` M.unsafeRead q ioff
  let j = nextIndex i
  c  <- fromIntegral `liftM` M.unsafeRead q coff
  qi <- fromIntegral `liftM` M.unsafeRead q i
  qj <- fromIntegral `liftM` M.unsafeRead q j
  let t   = aa * qi + c
      c'  = fromIntegral (t `shiftR` 32)
      x   = fromIntegral t + c'
      (# x', c'' #)  | x < c'    = (# x + 1, c' + 1 #)
                     | otherwise = (# x,     c' #)
      u   = aa * qj + fromIntegral c''
      d'  = fromIntegral (u `shiftR` 32)
      y   = fromIntegral u + d'
      (# y', d'' #)  | y < d'    = (# y + 1, d' + 1 #)
                     | otherwise = (# y,     d' #)
  M.unsafeWrite q i x'
  M.unsafeWrite q j y'
  M.unsafeWrite q ioff (fromIntegral j)
  M.unsafeWrite q coff (fromIntegral d'')
  return $! f x' y'
{-# INLINE uniform2 #-}

-- Type family for fixed size integrals. For signed data types it's
-- its unsigned couterpart with same size and for unsigned data types
-- it's same type
type family Unsigned a :: *

type instance Unsigned Int8  = Word8
type instance Unsigned Int16 = Word16
type instance Unsigned Int32 = Word32
type instance Unsigned Int64 = Word64

type instance Unsigned Word8  = Word8
type instance Unsigned Word16 = Word16
type instance Unsigned Word32 = Word32
type instance Unsigned Word64 = Word64

-- This is workaround for bug #25.
--
-- GHC-7.6 has a bug (#8072) which results in calculation of wrong
-- number of buckets in function `uniformRange'. Consequently uniformR
-- generates values in wrong range.
--
-- Bug only affects 32-bit systems and Int/Word data types. Word32
-- works just fine. So we set Word32 as unsigned counterpart for Int
-- and Word on 32-bit systems. It's done only for GHC-7.6 because
-- other versions are unaffected by the bug and we expect that GHC may
-- optimise code which uses Word better.
#if (WORD_SIZE_IN_BITS < 64) && (__GLASGOW_HASKELL__ == 706)
type instance Unsigned Int   = Word32
type instance Unsigned Word  = Word32
#else
type instance Unsigned Int   = Word
type instance Unsigned Word  = Word
#endif


-- Subtract two numbers under assumption that x>=y and store result in
-- unsigned data type of same size
sub :: (Integral a, Integral (Unsigned a)) => a -> a -> Unsigned a
sub x y = fromIntegral x - fromIntegral y
{-# INLINE sub #-}

add :: (Integral a, Integral (Unsigned a)) => a -> Unsigned a -> a
add m x = m + fromIntegral x
{-# INLINE add #-}

-- Generate uniformly distributed value in inclusive range.
--
-- NOTE: This function must be fully applied. Otherwise it won't be
--       inlined, which will cause a severe performance loss.
--
-- > uniformR     = uniformRange      -- won't be inlined
-- > uniformR a b = uniformRange a b  -- will be inlined
uniformRange :: ( Integral a, Bounded a, Variate a
                , Integral (Unsigned a), Bounded (Unsigned a), Variate (Unsigned a))
             => (a,a) -> Gen -> IO a
uniformRange (x1,x2) g
  | n == 0    = uniform g   -- Abuse overflow in unsigned types
  | otherwise = loop
  where
    -- Allow ranges where x2<x1
    (# i, j #) | x1 < x2   = (# x1, x2 #)
               | otherwise = (# x2, x1 #)
    n       = 1 + sub j i
    buckets = maxBound `div` n
    maxN    = buckets * n
    loop    = do x <- uniform g
                 if x < maxN then return $! add i (x `div` buckets)
                             else loop
{-# INLINE uniformRange #-}

-- | Generate a vector of pseudo-random variates.  This is not
-- necessarily faster than invoking 'uniform' repeatedly in a loop,
-- but it may be more convenient to use in some situations.
uniformVector :: (Variate a, Vector v a)
             => Gen -> Int -> IO (v a)
uniformVector gen n = G.replicateM n (uniform gen)
{-# INLINE uniformVector #-}

-- | Split a generator into several that can run independently.
splitGen :: Int -> GenIO -> IO [GenIO]
splitGen n gen
  | n <= 0    = return []
  | otherwise =
  fmap (gen:) . replicateM (n-1) $
  initialize =<< uniformVector gen 256

defaultSeed :: I.Vector Word32
defaultSeed = I.fromList [
  0x7042e8b3, 0x06f7f4c5, 0x789ea382, 0x6fb15ad8, 0x54f7a879, 0x0474b184,
  0xb3f8f692, 0x4114ea35, 0xb6af0230, 0xebb457d2, 0x47693630, 0x15bc0433,
  0x2e1e5b18, 0xbe91129c, 0xcc0815a0, 0xb1260436, 0xd6f605b1, 0xeaadd777,
  0x8f59f791, 0xe7149ed9, 0x72d49dd5, 0xd68d9ded, 0xe2a13153, 0x67648eab,
  0x48d6a1a1, 0xa69ab6d7, 0x236f34ec, 0x4e717a21, 0x9d07553d, 0x6683a701,
  0x19004315, 0x7b6429c5, 0x84964f99, 0x982eb292, 0x3a8be83e, 0xc1df1845,
  0x3cf7b527, 0xb66a7d3f, 0xf93f6838, 0x736b1c85, 0x5f0825c1, 0x37e9904b,
  0x724cd7b3, 0xfdcb7a46, 0xfdd39f52, 0x715506d5, 0xbd1b6637, 0xadabc0c0,
  0x219037fc, 0x9d71b317, 0x3bec717b, 0xd4501d20, 0xd95ea1c9, 0xbe717202,
  0xa254bd61, 0xd78a6c5b, 0x043a5b16, 0x0f447a25, 0xf4862a00, 0x48a48b75,
  0x1e580143, 0xd5b6a11b, 0x6fb5b0a4, 0x5aaf27f9, 0x668bcd0e, 0x3fdf18fd,
  0x8fdcec4a, 0x5255ce87, 0xa1b24dbf, 0x3ee4c2e1, 0x9087eea2, 0xa4131b26,
  0x694531a5, 0xa143d867, 0xd9f77c03, 0xf0085918, 0x1e85071c, 0x164d1aba,
  0xe61abab5, 0xb8b0c124, 0x84899697, 0xea022359, 0x0cc7fa0c, 0xd6499adf,
  0x746da638, 0xd9e5d200, 0xefb3360b, 0x9426716a, 0xabddf8c2, 0xdd1ed9e4,
  0x17e1d567, 0xa9a65000, 0x2f37dbc5, 0x9a4b8fd5, 0xaeb22492, 0x0ebe8845,
  0xd89dd090, 0xcfbb88c6, 0xb1325561, 0x6d811d90, 0x03aa86f4, 0xbddba397,
  0x0986b9ed, 0x6f4cfc69, 0xc02b43bc, 0xee916274, 0xde7d9659, 0x7d3afd93,
  0xf52a7095, 0xf21a009c, 0xfd3f795e, 0x98cef25b, 0x6cb3af61, 0x6fa0e310,
  0x0196d036, 0xbc198bca, 0x15b0412d, 0xde454349, 0x5719472b, 0x8244ebce,
  0xee61afc6, 0xa60c9cb5, 0x1f4d1fd0, 0xe4fb3059, 0xab9ec0f9, 0x8d8b0255,
  0x4e7430bf, 0x3a22aa6b, 0x27de22d3, 0x60c4b6e6, 0x0cf61eb3, 0x469a87df,
  0xa4da1388, 0xf650f6aa, 0x3db87d68, 0xcdb6964c, 0xb2649b6c, 0x6a880fa9,
  0x1b0c845b, 0xe0af2f28, 0xfc1d5da9, 0xf64878a6, 0x667ca525, 0x2114b1ce,
  0x2d119ae3, 0x8d29d3bf, 0x1a1b4922, 0x3132980e, 0xd59e4385, 0x4dbd49b8,
  0x2de0bb05, 0xd6c96598, 0xb4c527c3, 0xb5562afc, 0x61eeb602, 0x05aa192a,
  0x7d127e77, 0xc719222d, 0xde7cf8db, 0x2de439b8, 0x250b5f1a, 0xd7b21053,
  0xef6c14a1, 0x2041f80f, 0xc287332e, 0xbb1dbfd3, 0x783bb979, 0x9a2e6327,
  0x6eb03027, 0x0225fa2f, 0xa319bc89, 0x864112d4, 0xfe990445, 0xe5e2e07c,
  0xf7c6acb8, 0x1bc92142, 0x12e9b40e, 0x2979282d, 0x05278e70, 0xe160ba4c,
  0xc1de0909, 0x458b9bf4, 0xbfce9c94, 0xa276f72a, 0x8441597d, 0x67adc2da,
  0x6162b854, 0x7f9b2f4a, 0x0d995b6b, 0x193b643d, 0x399362b3, 0x8b653a4b,
  0x1028d2db, 0x2b3df842, 0x6eecafaf, 0x261667e9, 0x9c7e8cda, 0x46063eab,
  0x7ce7a3a1, 0xadc899c9, 0x017291c4, 0x528d1a93, 0x9a1ee498, 0xbb7d4d43,
  0x7837f0ed, 0x34a230cc, 0x614a628d, 0xb03f93b8, 0xd72e3b08, 0x604c98db,
  0x3cfacb79, 0x8b81646a, 0xc0f082fa, 0xd1f92388, 0xe5a91e39, 0xf95c756d,
  0x1177742f, 0xf8819323, 0x5c060b80, 0x96c1cd8f, 0x47d7b440, 0xbbb84197,
  0x35f749cc, 0x95b0e132, 0x8d90ad54, 0x5c3f9423, 0x4994005b, 0xb58f53b9,
  0x32df7348, 0x60f61c29, 0x9eae2f32, 0x85a3d398, 0x3b995dd4, 0x94c5e460,
  0x8e54b9f3, 0x87bc6e2a, 0x90bbf1ea, 0x55d44719, 0x2cbbfe6e, 0x439d82f0,
  0x4eb3782d, 0xc3f1e669, 0x61ff8d9e, 0x0909238d, 0xef406165, 0x09c1d762,
  0x705d184f, 0x188f2cc4, 0x9c5aa12a, 0xc7a5d70e, 0xbc78cb1b, 0x1d26ae62,
  0x23f96ae3, 0xd456bf32, 0xe4654f55, 0x31462bd8 ]
{-# NOINLINE defaultSeed #-}

-- $references
--
-- * Marsaglia, G. (2003) Seeds for random number generators.
--   /Communications of the ACM/ 46(5):90&#8211;93.
--   <http://doi.acm.org/10.1145/769800.769827>
--
-- * Doornik, J.A. (2007) Conversion of high-period random numbers to
--   floating point.
--   /ACM Transactions on Modeling and Computer Simulation/ 17(1).
--   <http://www.doornik.com/research/randomdouble.pdf>
