{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
	Copyright (C) 2011-2015 Dr. Alistair Ward

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
{- |
 [@AUTHOR@]	Dr. Alistair Ward

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' and defines /QuickCheck/-properties for "Math.Implementations.Primality".
-}

module Factory.Test.QuickCheck.Primality(
-- * Constants
	results
) where

import			Factory.Test.QuickCheck.PrimeFactorisation()
import qualified	Data.List
import qualified	Data.Numbers.Primes
import qualified	Factory.Math.Implementations.Primality		as Math.Implementations.Primality
import qualified	Factory.Math.Implementations.PrimeFactorisation	as Math.Implementations.PrimeFactorisation
import qualified	Factory.Math.Primality				as Math.Primality
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))

instance Test.QuickCheck.Arbitrary factorisationAlgorithm => Test.QuickCheck.Arbitrary (Math.Implementations.Primality.Algorithm factorisationAlgorithm)	where
	arbitrary	= Test.QuickCheck.oneof [
		Math.Implementations.Primality.AKS `fmap` Test.QuickCheck.arbitrary,
		return {-to Gen-monad-} Math.Implementations.Primality.MillerRabin
	 ]

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 50 } prop_prime,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 50 } prop_composite,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 50 } prop_consistency
 ] where
	prop_prime :: Math.Implementations.Primality.Algorithm Math.Implementations.PrimeFactorisation.Algorithm -> Integer -> Test.QuickCheck.Property
	prop_prime primalityAlgorithm i	= Test.QuickCheck.label "prop_prime" $ Math.Primality.isPrime primalityAlgorithm prime	where
		normalise n
			| primalityAlgorithm == Math.Implementations.Primality.MillerRabin	= n `mod` 1000000	-- Limited by the efficiency of 'Data.Numbers.Primes.primes'.
			| otherwise								= n `mod` 59

		prime :: Integer
		prime	= Data.List.genericIndex Data.Numbers.Primes.primes $ normalise i

	prop_composite :: Math.Implementations.Primality.Algorithm Math.Implementations.PrimeFactorisation.Algorithm -> [Integer] -> Test.QuickCheck.Property
	prop_composite primalityAlgorithm l	= length l > 1	==> Test.QuickCheck.label "prop_composite" . not $ Math.Primality.isPrime primalityAlgorithm composite	where
		normalise n
			| primalityAlgorithm == Math.Implementations.Primality.MillerRabin	= n `mod` 1000000
			| otherwise								= n `mod` 10

		composite :: Integer
		composite	= product . map (Data.List.genericIndex Data.Numbers.Primes.primes . normalise) $ take 8 l

	prop_consistency :: Math.Implementations.Primality.Algorithm Math.Implementations.PrimeFactorisation.Algorithm -> Math.Implementations.Primality.Algorithm Math.Implementations.PrimeFactorisation.Algorithm -> Integer -> Test.QuickCheck.Property
	prop_consistency l r i	= l /= r	==> Test.QuickCheck.label "prop_consistency" $ Math.Primality.isPrime l i' == Math.Primality.isPrime r i'	where
		i'	= i `mod` 512

