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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' and defines /QuickCheck/-properties for "Math.PrimeFactorisation".
-}

module Factory.Test.QuickCheck.PrimeFactorisation(
-- * Constants
	results
) where

import qualified	Data.List
import qualified	Data.Numbers.Primes
import qualified	Factory.Data.PrimeFactors			as Data.PrimeFactors
import qualified	Factory.Data.Exponential			as Data.Exponential
import qualified	Factory.Math.Implementations.PrimeFactorisation	as Math.Implementations.PrimeFactorisation
import qualified	Factory.Math.MultiplicativeOrder		as Math.MultiplicativeOrder
import qualified	Factory.Math.PrimeFactorisation			as Math.PrimeFactorisation
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))

instance Test.QuickCheck.Arbitrary Math.Implementations.PrimeFactorisation.Algorithm	where
	arbitrary	= Test.QuickCheck.oneof [
		Test.QuickCheck.elements [
			Math.Implementations.PrimeFactorisation.TrialDivision,
			Math.Implementations.PrimeFactorisation.FermatsMethod
		]
	 ]

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	Test.QuickCheck.quickCheckResult prop_consistency,
	Test.QuickCheck.quickCheckResult prop_primeFactors,
	Test.QuickCheck.quickCheckResult prop_smoothness,
	Test.QuickCheck.quickCheckResult prop_eulersTotientP,
	Test.QuickCheck.quickCheckResult prop_eulersTotientInequality,
	Test.QuickCheck.quickCheckResult prop_eulersTotient,
	Test.QuickCheck.quickCheckResult prop_lagrange,
	Test.QuickCheck.quickCheckResult prop_multiplicativeOrder,
	Test.QuickCheck.quickCheckResult prop_perfectPower
 ] where
	prop_consistency :: Integer -> Test.QuickCheck.Property
	prop_consistency i	= Test.QuickCheck.label "prop_consistency" $ (Math.PrimeFactorisation.primeFactors Math.Implementations.PrimeFactorisation.TrialDivision i' :: Data.PrimeFactors.Factors Integer Int) == Math.PrimeFactorisation.primeFactors Math.Implementations.PrimeFactorisation.FermatsMethod i'	where
		i' :: Integer
		i'	= succ $ i `mod` 1000000

	prop_primeFactors, prop_smoothness, prop_eulersTotientP, prop_eulersTotientInequality :: Math.Implementations.PrimeFactorisation.Algorithm -> Integer -> Test.QuickCheck.Property
	prop_primeFactors algorithm i	= Test.QuickCheck.label "prop_primeFactors" $ Data.PrimeFactors.product' (recip 2) {-TODO-} 10 (Math.PrimeFactorisation.primeFactors algorithm i') == i'	where
		i' :: Integer
		i'	= succ $ i `mod` 1000000

	prop_smoothness algorithm i	= Test.QuickCheck.label "prop_smoothness" $ (Math.PrimeFactorisation.smoothness algorithm !! (2 ^ i')) <= (2 :: Integer)	where
		i' :: Integer
		i'	= i `mod` 20

	prop_eulersTotientP algorithm i	= Test.QuickCheck.label "prop_eulersTotientP" $ Math.PrimeFactorisation.eulersTotient algorithm prime == pred prime	where
		prime :: Integer
		prime	= Data.List.genericIndex Data.Numbers.Primes.primes (i `mod` 10000)

	prop_eulersTotientInequality algorithm i	= i `notElem` [2, 6]	==> Test.QuickCheck.label "prop_eulersTotientInequality" $ Math.PrimeFactorisation.eulersTotient algorithm i' >= floor (sqrt $ fromIntegral i' :: Double)	where
		i'	= succ $ i `mod` 100000

	prop_eulersTotient, prop_lagrange, prop_multiplicativeOrder, prop_perfectPower :: Math.Implementations.PrimeFactorisation.Algorithm -> Integer -> Integer -> Test.QuickCheck.Property
	prop_eulersTotient algorithm i power	= Test.QuickCheck.label "prop_eulersTotient" $ Math.PrimeFactorisation.eulersTotient algorithm (base ^ power') == (base ^ pred power') * pred base	where
		base :: Integer
		base	= Data.List.genericIndex Data.Numbers.Primes.primes (i `mod` 8)

		power'	= succ $ power `mod` 5

	prop_lagrange algorithm base modulus	= gcd base modulus' == 1	==> Test.QuickCheck.label "prop_lagrange" $ (Math.PrimeFactorisation.eulersTotient algorithm modulus' `rem` Math.MultiplicativeOrder.multiplicativeOrder algorithm base modulus') == 0	where
		modulus' :: Integer
		modulus'	= 2 + abs modulus

	prop_multiplicativeOrder algorithm base modulus	= gcd base modulus' == 1	==> Test.QuickCheck.label "prop_multiplicativeOrder" $ (
		base ^ Math.MultiplicativeOrder.multiplicativeOrder algorithm base modulus'
	 ) `mod` modulus' == 1	where
		modulus' :: Integer
		modulus'	= 2 + abs modulus

	prop_perfectPower algorithm b e	= Test.QuickCheck.label "prop_perfectPower" $ foldr1 gcd (
		map Data.Exponential.getExponent . Math.PrimeFactorisation.primeFactors algorithm $ (2 + b `mod` 10 :: Integer) ^ (2 + e `mod` 5)
	 ) > 1
