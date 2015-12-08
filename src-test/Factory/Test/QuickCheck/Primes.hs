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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' and defines /QuickCheck/-properties for "Math.Implementations.Primes".
-}

module Factory.Test.QuickCheck.Primes(
-- * Constants
--	defaultAlgorithm,
	results,
-- * Functions
--	isPrime,
	upperBound
) where

import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	Data.Set
import qualified	Factory.Data.PrimeWheel				as Data.PrimeWheel
import qualified	Factory.Math.Implementations.Primality		as Math.Implementations.Primality
import qualified	Factory.Math.Implementations.PrimeFactorisation	as Math.Implementations.PrimeFactorisation
import qualified	Factory.Math.Implementations.Primes.Algorithm	as Math.Implementations.Primes.Algorithm
import qualified	Factory.Math.Primality				as Math.Primality
import qualified	Factory.Math.Primes				as Math.Primes
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))

instance Test.QuickCheck.Arbitrary Math.Implementations.Primes.Algorithm.Algorithm	where
	arbitrary	= Test.QuickCheck.oneof [
		return {-to Gen-monad-} Math.Implementations.Primes.Algorithm.TurnersSieve,
		(Math.Implementations.Primes.Algorithm.TrialDivision . (`mod` 10)) `fmap` Test.QuickCheck.arbitrary,
		(Math.Implementations.Primes.Algorithm.SieveOfEratosthenes . (`mod` 10)) `fmap` Test.QuickCheck.arbitrary
	 ]

isPrime :: (Control.DeepSeq.NFData i, Integral i, Show i) => i -> Bool
isPrime	= Math.Primality.isPrime primalityAlgorithm	where
	primalityAlgorithm :: Math.Implementations.Primality.Algorithm Math.Implementations.PrimeFactorisation.Algorithm
	primalityAlgorithm	= Data.Default.def

upperBound :: Math.Implementations.Primes.Algorithm.Algorithm -> Int -> Int
upperBound algorithm i	= mod i $ if algorithm == Math.Implementations.Primes.Algorithm.TurnersSieve
	then 8192
	else 65536

defaultAlgorithm :: Math.Implementations.Primes.Algorithm.Algorithm
defaultAlgorithm	= Data.Default.def

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	Test.QuickCheck.quickCheckResult prop_isPrime,
	Test.QuickCheck.quickCheckResult prop_isComposite,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 50 } prop_consistency,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 25 } prop_rewriteRule,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 25 } prop_sieveOfAtkin,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 25 } prop_sieveOfAtkinRewrite
 ] where
	prop_isPrime, prop_isComposite :: Math.Implementations.Primes.Algorithm.Algorithm -> Int -> Test.QuickCheck.Property
	prop_isPrime algorithm i	= Test.QuickCheck.label "prop_isPrime" . all isPrime . takeWhile (<= upperBound algorithm i) $ (Math.Primes.primes algorithm :: [Int])
	prop_isComposite algorithm i	= Test.QuickCheck.label "prop_isComposite" . not . any isPrime . Data.Set.toList . Data.Set.difference (
		Data.Set.fromList [2 .. upperBound algorithm i]
	 ) . Data.Set.fromList . takeWhile (<= upperBound algorithm i) $ Math.Primes.primes algorithm

	prop_consistency :: Math.Implementations.Primes.Algorithm.Algorithm -> Math.Implementations.Primes.Algorithm.Algorithm -> Int -> Test.QuickCheck.Property
	prop_consistency l r i = l /= r	==> Test.QuickCheck.label "prop_consistency" . and . take (i `mod` 4096) $ zipWith (==) (Math.Primes.primes l) (Math.Primes.primes r :: [Int])

	prop_rewriteRule :: Data.PrimeWheel.NPrimes -> Int -> Test.QuickCheck.Property
	prop_rewriteRule wheelSize i	= Test.QuickCheck.label "prop_rewriteRule" $ toInteger (Math.Primes.primes (Math.Implementations.Primes.Algorithm.SieveOfEratosthenes wheelSize') !! index :: Int) == (Math.Primes.primes (Math.Implementations.Primes.Algorithm.SieveOfEratosthenes wheelSize') !! index :: Integer)	where
		wheelSize'	= wheelSize `mod` 8
		index		= i `mod` 131072

	prop_sieveOfAtkin, prop_sieveOfAtkinRewrite :: Int -> Test.QuickCheck.Property
	prop_sieveOfAtkin i	= Test.QuickCheck.label "prop_sieveOfAtkin" $ Math.Primes.primes (Math.Implementations.Primes.Algorithm.SieveOfAtkin prime) !! index == prime	where
		index	= i `mod` 131072

		prime :: Integer
		prime	= Math.Primes.primes defaultAlgorithm !! index

	prop_sieveOfAtkinRewrite i	= Test.QuickCheck.label "prop_sieveOfAtkinRewrite" $ Math.Primes.primes (Math.Implementations.Primes.Algorithm.SieveOfAtkin $ fromIntegral prime) !! index == prime	where
		index	= i `mod` 131072

		prime :: Int
		prime	= Math.Primes.primes defaultAlgorithm !! index

