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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' and defines /QuickCheck/-properties for "Data.MonicPolynomial".
-}

module Factory.Test.QuickCheck.MonicPolynomial(
-- * Constants
	results,
-- * Types
-- ** Type-synonyms
--	P
) where

import			Factory.Data.Ring((=*=), (=+=), (=^))
import			Factory.Test.QuickCheck.Polynomial()
import qualified	Factory.Data.MonicPolynomial	as Data.MonicPolynomial
import qualified	Factory.Data.Polynomial		as Data.Polynomial
import qualified	Factory.Data.QuotientRing	as Data.QuotientRing
import qualified	Factory.Data.Ring		as Data.Ring
import qualified	Test.QuickCheck

instance (
	Integral			c,
	Integral			e,
	Test.QuickCheck.Arbitrary	c,
	Test.QuickCheck.Arbitrary	e,
	Show				c,
	Show				e
 ) => Test.QuickCheck.Arbitrary (Data.MonicPolynomial.MonicPolynomial c e)	where
	arbitrary	= do
		polynomial	<- Test.QuickCheck.arbitrary

		return {-to Gen-monad-} . Data.MonicPolynomial.mkMonicPolynomial $ ((1, succ $ Data.Polynomial.getDegree polynomial) :) `Data.Polynomial.lift` polynomial

type P	= Data.MonicPolynomial.MonicPolynomial Integer Integer

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	Test.QuickCheck.quickCheckResult prop_quotRem,
	Test.QuickCheck.quickCheckResult prop_quotientRingNormalised,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 50 } prop_perfectPower,
	Test.QuickCheck.quickCheckResult prop_isDivisibleBy
 ] where
	prop_quotRem, prop_quotientRingNormalised :: P -> P -> Test.QuickCheck.Property
	prop_quotRem numerator denominator	= Test.QuickCheck.label "prop_quotRem" $ numerator == denominator =*= quotient =+= remainder	where
		(quotient, remainder)	= numerator `Data.QuotientRing.quotRem'` denominator

	prop_quotientRingNormalised numerator denominator	= Test.QuickCheck.label "prop_quotientRingNormalised" $ all (Data.Polynomial.isNormalised . Data.MonicPolynomial.getPolynomial) [numerator `Data.QuotientRing.quot'` denominator, numerator `Data.QuotientRing.rem'` denominator]

	prop_perfectPower :: P -> Int -> Test.QuickCheck.Property
	prop_perfectPower polynomial power	= Test.QuickCheck.label "prop_perfectPower" $ iterate (`Data.QuotientRing.quot'` polynomial) (polynomial =^ power') !! pred power' == polynomial	where
		power' :: Int
		power'	= succ $ power `mod` 100

	prop_isDivisibleBy :: [P] -> Test.QuickCheck.Property
	prop_isDivisibleBy monicPolynomials	= Test.QuickCheck.label "prop_isDivisibleBy" $ all (Data.QuotientRing.isDivisibleBy (Data.Ring.product' (recip 2) {-TODO-} 10 monicPolynomials)) monicPolynomials


