{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
	Copyright (C) 2011-2017 Dr. Alistair Ward

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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' and defines /QuickCheck/-properties for "Data.Polynomial".
-}

module Factory.Test.QuickCheck.Polynomial(
-- * Constants
	results
) where

import			Control.Arrow((&&&), (***))
import			Factory.Data.Ring((=*=), (=+=), (=-=), (=^))
import qualified	Data.Numbers.Primes
import qualified	Factory.Data.Polynomial		as Data.Polynomial
import qualified	Factory.Data.QuotientRing	as Data.QuotientRing
import qualified	Factory.Data.Ring		as Data.Ring
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))

instance (
	Integral			c,
	Integral			e,
	Test.QuickCheck.Arbitrary	c,
	Test.QuickCheck.Arbitrary	e
 ) => Test.QuickCheck.Arbitrary (Data.Polynomial.Polynomial c e)	where
	arbitrary	= (Data.Polynomial.mkPolynomial . map (subtract 4 . (`mod` 8) *** (`mod` 8))) `fmap` Test.QuickCheck.arbitrary

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	Test.QuickCheck.quickCheckResult prop_congruence,
	Test.QuickCheck.quickCheckResult prop_quotRem,
	Test.QuickCheck.quickCheckResult prop_degree,
	Test.QuickCheck.quickCheckResult prop_ringNormalised,
	Test.QuickCheck.quickCheckResult prop_quotientRingNormalised,
	Test.QuickCheck.quickCheckResult prop_power,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 50 } prop_perfectPower,
	Test.QuickCheck.quickCheckResult prop_normalised,
	Test.QuickCheck.quickCheckResult prop_raiseModuloNormalised,
	Test.QuickCheck.quickCheckResult prop_integralDomain,
	Test.QuickCheck.quickCheckResult prop_isDivisibleBy
 ] where
	prop_congruence :: Int -> Test.QuickCheck.Property
	prop_congruence i	= Test.QuickCheck.label "prop_congruence" $ Data.Polynomial.areCongruentModulo (Data.Polynomial.mkLinear 1 (negate 1) =^ prime) (Data.Polynomial.mkPolynomial [(1, prime), (negate 1, 0)]) prime	where
		prime :: Integer
		prime	= Data.Numbers.Primes.primes !! mod i 100

	prop_quotRem, prop_degree, prop_ringNormalised, prop_quotientRingNormalised :: Data.Polynomial.Polynomial Integer Integer -> Data.Polynomial.Polynomial Integer Integer -> Test.QuickCheck.Property
	prop_quotRem numerator denominator	= denominator' /= Data.Polynomial.zero	==> Test.QuickCheck.label "prop_quotRem" $ numerator' == denominator' =*= quotient =+= remainder	where
		numerator', denominator' :: Data.Polynomial.Polynomial Rational Integer
		(numerator', denominator')	= ($ numerator) &&& ($ denominator) $ Data.Polynomial.realCoefficientsToFrac 

		(quotient, remainder)	= numerator' `Data.QuotientRing.quotRem'` denominator'

	prop_degree numerator denominator	= denominator' /= Data.Polynomial.zero	==> Test.QuickCheck.label "prop_degree" $ remainder == Data.Polynomial.zero || Data.Polynomial.getDegree remainder < Data.Polynomial.getDegree denominator'	where
		numerator', denominator' :: Data.Polynomial.Polynomial Rational Integer
		(numerator', denominator')	= ($ numerator) &&& ($ denominator) $ Data.Polynomial.realCoefficientsToFrac 

		remainder	= numerator' `Data.QuotientRing.rem'` denominator'

	prop_ringNormalised l r	= Test.QuickCheck.label "prop_ringNormalised" $ all Data.Polynomial.isNormalised [l =*= r, l =+= r, l =-= r]

	prop_quotientRingNormalised numerator denominator	= denominator' /= Data.Polynomial.zero	==> Test.QuickCheck.label "prop_quotientRingNormalised" $ all Data.Polynomial.isNormalised [numerator' `Data.QuotientRing.quot'` denominator', numerator' `Data.QuotientRing.rem'` denominator']	where
		numerator', denominator' :: Data.Polynomial.Polynomial Rational Integer
		(numerator', denominator')	= ($ numerator) &&& ($ denominator) $ Data.Polynomial.realCoefficientsToFrac 

	prop_power, prop_perfectPower, prop_normalised :: Data.Polynomial.Polynomial Integer Integer -> Int -> Test.QuickCheck.Property
	prop_power polynomial power	= Test.QuickCheck.label "prop_power" $ polynomial =^ power' == iterate (=*= polynomial) polynomial !! pred power'	where
		power' :: Int
		power'	= succ $ power `mod` 100

	prop_perfectPower polynomial power	= polynomial' /= Data.Polynomial.zero	==> Test.QuickCheck.label "prop_perfectPower" $ iterate (`Data.QuotientRing.quot'` polynomial') (polynomial' =^ power') !! pred power' == polynomial'	where
		polynomial' :: Data.Polynomial.Polynomial Rational Integer
		polynomial'	= Data.Polynomial.realCoefficientsToFrac polynomial

		power' :: Int
		power'	= succ $ power `mod` 100

	prop_normalised polynomial i	= Test.QuickCheck.label "prop_normalised" $ all Data.Polynomial.isNormalised [
		polynomial =^ power',
		polynomial `Data.Polynomial.mod'` modulus'
	 ] where
		power' :: Int
		power'	= succ $ i `mod` 100

		modulus' :: Integer
		modulus'	= succ $ fromIntegral i `mod` 100

	prop_raiseModuloNormalised :: Data.Polynomial.Polynomial Integer Integer -> Integer -> Integer -> Test.QuickCheck.Property
	prop_raiseModuloNormalised polynomial power modulus	= Test.QuickCheck.label "prop_raiseModuloNormalised" . Data.Polynomial.isNormalised $ Data.Polynomial.raiseModulo polynomial power' modulus'	where
		power', modulus' :: Integer
		power'		= succ $ power `mod` 100
		modulus'	= succ $ modulus `mod` 100

	prop_integralDomain, prop_isDivisibleBy :: [Data.Polynomial.Polynomial Integer Integer] -> Test.QuickCheck.Property
	prop_integralDomain polynomials	= Data.Polynomial.zero `notElem` polynomials	==> Test.QuickCheck.label "prop_integralDomain" $ Data.Ring.product' (recip 2) {-TODO-} 10 polynomials /= Data.Polynomial.zero

	prop_isDivisibleBy polynomials	= Test.QuickCheck.label "prop_isDivisibleBy" . all (Data.QuotientRing.isDivisibleBy (Data.Ring.product' (recip 2) {-TODO-} 10 polynomials')) $ filter (/= Data.Polynomial.zero) polynomials'	where
		polynomials' :: [Data.Polynomial.Polynomial Rational Integer]
		polynomials'	= map Data.Polynomial.realCoefficientsToFrac polynomials

