{-# LANGUAGE CPP #-}
-- |
-- Module    : Numeric.SpecFunctions
-- Copyright : (c) 2009, 2011, 2012 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
module Numeric.SpecFunctions (
    erf
  , erfc
  , invErf
  , invErfc
  , log1p
  , log1pmx
  , log2
  , expm1
  ) where

import Numeric.SpecFunctions.Internal
#if MIN_VERSION_base(4,9,0)
import GHC.Float (log1p,expm1)
#endif

-- $log1p
--
-- Base starting from @4.9.0@ (GHC 8.0) provides 'log1p' and 'expm1'
-- as method of class 'Floating'. In this case we simply reexport
-- these function. Otherwise we provide our own with more restrictive
-- signature @Double → Double@.

-- $references
--
-- * Bernardo, J. (1976) Algorithm AS 103: Psi (digamma)
--   function. /Journal of the Royal Statistical Society. Series C
--   (Applied Statistics)/ 25(3):315-317.
--   <http://www.jstor.org/stable/2347257>
--
-- * Cran, G.W., Martin, K.J., Thomas, G.E. (1977) Remark AS R19
--   and Algorithm AS 109: A Remark on Algorithms: AS 63: The
--   Incomplete Beta Integral AS 64: Inverse of the Incomplete Beta
--   Function Ratio. /Journal of the Royal Statistical Society. Series
--   C (Applied Statistics)/ Vol. 26, No. 1 (1977), pp. 111-114
--   <http://www.jstor.org/pss/2346887>
--
-- * Lanczos, C. (1964) A precision approximation of the gamma
--   function.  /SIAM Journal on Numerical Analysis B/
--   1:86&#8211;96. <http://www.jstor.org/stable/2949767>
--
-- * Loader, C. (2000) Fast and Accurate Computation of Binomial
--   Probabilities. <http://projects.scipy.org/scipy/raw-attachment/ticket/620/loader2000Fast.pdf>
--
-- * Macleod, A.J. (1989) Algorithm AS 245: A robust and reliable
--   algorithm for the logarithm of the gamma function.
--   /Journal of the Royal Statistical Society, Series C (Applied Statistics)/
--   38(2):397&#8211;402. <http://www.jstor.org/stable/2348078>
--
-- * Majumder, K.L., Bhattacharjee, G.P. (1973) Algorithm AS 63: The
--   Incomplete Beta Integral. /Journal of the Royal Statistical
--   Society. Series C (Applied Statistics)/ Vol. 22, No. 3 (1973),
--   pp. 409-411. <http://www.jstor.org/pss/2346797>
--
-- * Majumder, K.L., Bhattacharjee, G.P. (1973) Algorithm AS 64:
--   Inverse of the Incomplete Beta Function Ratio. /Journal of the
--   Royal Statistical Society. Series C (Applied Statistics)/
--   Vol. 22, No. 3 (1973), pp. 411-414
--   <http://www.jstor.org/pss/2346798>
--
-- * Temme, N.M. (1992) Asymptotic inversion of the incomplete beta
--   function. /Journal of Computational and Applied Mathematics
--   41(1992) 145-157.
--
-- * Temme, N.M. (1994) A set of algorithms for the incomplete gamma
--   functions. /Probability in the Engineering and Informational
--   Sciences/, 8, 1994, 291-307. Printed in the U.S.A.

