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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' and defines /QuickCheck/-properties for "Math.Implementations.SquareRoot".
-}

module Factory.Test.QuickCheck.SquareRoot(
-- * Constants
	results
) where

import			Data.Ratio((%))
import qualified	Data.Ratio
import qualified	Factory.Math.Implementations.SquareRoot	as Math.Implementations.SquareRoot
import qualified	Factory.Math.Power			as Math.Power
import qualified	Factory.Math.Precision			as Math.Precision
import qualified	Factory.Math.SquareRoot			as Math.SquareRoot
import qualified	Test.QuickCheck

instance Test.QuickCheck.Arbitrary Math.Implementations.SquareRoot.Algorithm	where
	arbitrary	= Test.QuickCheck.oneof [
		Test.QuickCheck.elements [
			Math.Implementations.SquareRoot.BakhshaliApproximation,
			Math.Implementations.SquareRoot.ContinuedFraction,
			Math.Implementations.SquareRoot.HalleysMethod,
			Math.Implementations.SquareRoot.NewtonRaphsonIteration
		],
		Math.Implementations.SquareRoot.TaylorSeries `fmap` Test.QuickCheck.elements [2 .. 32]
	 ]

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= mapM Test.QuickCheck.quickCheckResult [
--	prop_accuracy,	-- This occasionally fails.
	prop_factorable
--	prop_perfectSquare	-- This occasionally fails.
 ] where
	prop_accuracy, prop_factorable, prop_perfectSquare :: (Math.Implementations.SquareRoot.Algorithm, Math.Precision.DecimalDigits, Rational) -> Test.QuickCheck.Property
	prop_accuracy (algorithm, decimalDigits, operand)	= Test.QuickCheck.label "prop_accuracy" . (>= requiredDecimalDigits) . Math.SquareRoot.getAccuracy operand' $ Math.SquareRoot.squareRoot algorithm requiredDecimalDigits operand'	where
		requiredDecimalDigits :: Math.Precision.DecimalDigits
		requiredDecimalDigits	= succ $ decimalDigits `mod` 1024

		operand' :: Rational
		operand'	= abs operand

	prop_factorable (algorithm, decimalDigits, operand)	= Test.QuickCheck.label "prop_factorable" . (<= 5) . (
		* 10 ^ requiredDecimalDigits	-- Promote the relative error.
	 ) . abs $ 1 - (
		Math.SquareRoot.squareRoot algorithm requiredDecimalDigits (
			toRational $ Data.Ratio.numerator operand'
		) / Math.SquareRoot.squareRoot algorithm requiredDecimalDigits (
			toRational $ Data.Ratio.denominator operand'
		)
	 ) / Math.SquareRoot.squareRoot algorithm requiredDecimalDigits operand' where
		requiredDecimalDigits :: Math.Precision.DecimalDigits
		requiredDecimalDigits	= succ $ decimalDigits `mod` 1024

		operand' :: Rational
		operand'	= succ $ abs operand

	prop_perfectSquare (algorithm, decimalDigits, operand)	= Test.QuickCheck.label "prop_perfectSquare" . Math.SquareRoot.isPrecise perfectSquare $ Math.SquareRoot.squareRoot algorithm requiredDecimalDigits perfectSquare	where
		requiredDecimalDigits :: Math.Precision.DecimalDigits
		requiredDecimalDigits	= succ $ decimalDigits `mod` 32768

		operand', perfectSquare :: Rational
		operand'	= (abs (Data.Ratio.numerator operand) `min` (2 ^ (32 :: Int))) % (abs (Data.Ratio.denominator operand) `min` (2 ^ (32 :: Int)))	-- Avoid floating-point rounding-errors in 'Math.SquareRoot.rSqrt'.
		perfectSquare	= Math.Power.square operand'

