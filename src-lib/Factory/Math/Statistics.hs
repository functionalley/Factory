{-
	Copyright (C) 2011 Dr. Alistair Ward

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

 [@DESCRIPTION@]	Miscellaneous statistics functions.
-}

module Factory.Math.Statistics(
-- * Functions
	getMean,
	getWeightedMean,
--	getDispersionFromMean,
	getVariance,
	getStandardDeviation,
	getAverageAbsoluteDeviation,
	getCoefficientOfVariance,
	nCr,
	nPr
) where

import			Control.Arrow((***))
import			Control.Parallel(par, pseq)
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Factory.Math.Factorial			as Math.Factorial
import qualified	Factory.Math.Implementations.Factorial	as Math.Implementations.Factorial
import qualified	Factory.Math.Power			as Math.Power

{- |
	* Determines the /mean/ of the specified numbers; <http://en.wikipedia.org/wiki/Mean>.

	* Should the caller define the result-type as 'Rational', then it will be free from rounding-errors.
-}
getMean :: (
	Data.Foldable.Foldable	foldable,
	Fractional		result,
	Real			value
 )
	=> foldable value
	-> result
getMean foldable
	| denominator == 0	= error "Factory.Math.Statistics.getMean:\tno data => undefined result."
	| otherwise		= realToFrac numerator / fromIntegral denominator
	where
		(numerator, denominator)	= Data.Foldable.foldr (\s -> (+ s) *** succ) (0, 0 :: Int) foldable

{- |
	* Determines the /weighted mean/ of the specified numbers; <http://en.wikipedia.org/wiki/Weighted_arithmetic_mean>.

	* The specified value is only evaluated if the corresponding weight is non-zero.

	* Should the caller define the result-type as 'Rational', then it will be free from rounding-errors.
-}
getWeightedMean :: (
	Data.Foldable.Foldable	foldable,
	Fractional		result,
	Real			value,
	Real			weight
 )
	=> foldable (value, weight)	-- ^ Each pair consists of a value & the corresponding weight.
	-> result
getWeightedMean foldable
	| denominator == 0	= error "Factory.Math.Statistics.getWeightedMean:\tzero weight => undefined result."
	| otherwise		= numerator / realToFrac denominator
	where
		(numerator, denominator)	= Data.Foldable.foldr (
			\(value, weight)	-> if weight == 0
				then id	--Avoid unnecessarily evaluation.
				else (+ realToFrac value * realToFrac weight) *** (+ weight)
		 ) (0, 0) foldable

{- |
	* Measures the /dispersion/ of a /population/ of results from the /mean/ value; <http://en.wikipedia.org/wiki/Statistical_dispersion>.

	* Should the caller define the result-type as 'Rational', then it will be free from rounding-errors.
-}
getDispersionFromMean :: (
	Data.Foldable.Foldable	foldable,
	Fractional		result,
	Functor			foldable,
	Real			value
 ) => (Rational -> Rational) -> foldable value -> result
getDispersionFromMean weight foldable	= getMean $ fmap (weight . (+ negate mean) . toRational) foldable	where
	mean :: Rational
	mean	= getMean foldable

{- |
	* Determines the exact /variance/ of the specified numbers; <http://en.wikipedia.org/wiki/Variance>.

	* Should the caller define the result-type as 'Rational', then it will be free from rounding-errors.
-}
getVariance :: (
	Data.Foldable.Foldable	foldable,
	Fractional		variance,
	Functor			foldable,
	Real			value
 ) => foldable value -> variance
getVariance	= getDispersionFromMean Math.Power.square

-- | Determines the /standard-deviation/ of the specified numbers; <http://en.wikipedia.org/wiki/Standard_deviation>.
getStandardDeviation :: (
	Data.Foldable.Foldable	foldable,
	Floating		result,
	Functor			foldable,
	Real			value
 ) => foldable value -> result
getStandardDeviation	= sqrt . getVariance

{- |
	* Determines the /average absolute deviation/ of the specified numbers; <http://en.wikipedia.org/wiki/Absolute_deviation#Average_absolute_deviation>.

	* Should the caller define the result-type as 'Rational', then it will be free from rounding-errors.
-}
getAverageAbsoluteDeviation :: (
	Data.Foldable.Foldable	foldable,
	Fractional		result,
	Functor			foldable,
	Real			value
 ) => foldable value -> result
getAverageAbsoluteDeviation	= getDispersionFromMean abs

-- | Determines the /coefficient-of-variance/ of the specified numbers; <http://en.wikipedia.org/wiki/Coefficient_of_variation>.
getCoefficientOfVariance :: (
	Data.Foldable.Foldable	foldable,
	Eq			result,
	Floating		result,
	Functor			foldable,
	Real			value
 ) => foldable value -> result
getCoefficientOfVariance l
	| mean == 0	= error "Factory.Math.Statistics.getCoefficientOfVariance:\tundefined if mean is zero."
	| otherwise	= getStandardDeviation l / abs mean
	where
		mean	= getMean l

-- | The number of unordered /combinations/ of /r/ objects taken from /n/; <http://en.wikipedia.org/wiki/Combination>.
nCr :: (Math.Factorial.Algorithmic factorialAlgorithm, Integral i, Show i)
	=> factorialAlgorithm
	-> i	-- ^ The total number of items from which to select.
	-> i	-- ^ The number of items in a sample.
	-> i	-- ^ The number of combinations.
nCr _ 0 _	= 1
nCr _ _ 0	= 1
nCr factorialAlgorithm n r
	| n < 0		= error $ "Factory.Math.Statistics.nCr:\tinvalid n; " ++ show n
	| r < 0		= error $ "Factory.Math.Statistics.nCr:\tinvalid r; " ++ show r
	| n < r		= 0
	| otherwise	= numerator `par` (denominator `pseq` numerator `div` denominator)
	where
		[smaller, bigger]	= Data.List.sort [r, n - r]
		numerator		= Math.Implementations.Factorial.risingFactorial (succ bigger) (n - bigger)
		denominator		= Math.Factorial.factorial factorialAlgorithm smaller

-- | The number of /permutations/ of /r/ objects taken from /n/; <http://en.wikipedia.org/wiki/Permutations>.
nPr :: (Integral i, Show i)
	=> i	-- ^ The total number of items from which to select.
	-> i	-- ^ The number of items in a sample.
	-> i	-- ^ The number of permutations.
nPr 0 _	= 1
nPr _ 0	= 1
nPr n r
	| n < 0		= error $ "Factory.Math.Statistics.nPr:\tinvalid n; " ++ show n
	| r < 0		= error $ "Factory.Math.Statistics.nPr:\tinvalid r; " ++ show r
	| n < r		= 0
	| otherwise	= Math.Implementations.Factorial.fallingFactorial n r

