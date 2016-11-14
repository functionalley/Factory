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
	getRootMeanSquare,
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
import qualified	Control.Exception
import			Control.Parallel(par, pseq)
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Factory.Math.Factorial			as Math.Factorial
import qualified	Factory.Math.Implementations.Factorial	as Math.Implementations.Factorial
import qualified	Factory.Math.Power			as Math.Power

{- |
	* Determines the /mean/ of the specified numbers; <https://en.wikipedia.org/wiki/Mean>.

	* Should the caller define the result-type as 'Rational', then it will be free from rounding-errors.
-}
getMean :: (
	Data.Foldable.Foldable	foldable,
	Fractional		result,
	Real			value
 )
	=> foldable value
	-> result
getMean foldable	= Control.Exception.assert (denominator /= 0) $ realToFrac numerator / fromIntegral denominator	where
	denominator :: Int
	(numerator, denominator)	= Data.Foldable.foldl' (
		\acc x	-> let
			acc'@(n, d)	= (+ x) *** succ $ acc
		in n `seq` d `seq` acc'
	 ) (0, 0) foldable

-- | Determines the /root mean square/ of the specified numbers; <https://en.wikipedia.org/wiki/Root_mean_square>.
getRootMeanSquare :: (
	Data.Foldable.Foldable	foldable,
	Floating		result,
	Real			value
 )
	=> foldable value
	-> result
getRootMeanSquare foldable	= Control.Exception.assert (denominator /= 0) $ sqrt $ realToFrac numerator / fromIntegral denominator	where
	denominator :: Int
	(numerator, denominator)	= Data.Foldable.foldl' (
		\acc x -> let
			acc'@(n, d)	= (+ Math.Power.square x) *** succ $ acc
		 in n `seq` d `seq` acc'
	 ) (0, 0) foldable

{- |
	* Determines the /weighted mean/ of the specified numbers; <https://en.wikipedia.org/wiki/Weighted_arithmetic_mean>.

	* The specified value is only evaluated if the corresponding weight is non-zero.

	* Should the caller define the result-type as 'Rational', then it will be free from rounding-errors.

	* CAVEAT: because the operand is more general than a list, no optimisation is performed when supplied a singleton.
-}
getWeightedMean :: (
	Data.Foldable.Foldable	foldable,
	Eq			result,
	Fractional		result,
	Real			value,
	Real			weight
 )
	=> foldable (value, weight)	-- ^ Each pair consists of a value & the corresponding weight.
	-> result
getWeightedMean foldable = Control.Exception.assert (denominator /= 0) $ numerator / denominator	where
	(numerator, denominator)	= Data.Foldable.foldl' (
		\acc (value, weight)	-> case realToFrac weight of
			0	-> acc	-- Avoid unnecessarily evaluation.
			w	-> let
				acc'@(n, d)	= (+ realToFrac value * w) *** (+ w) $ acc	-- Perform the arithmetic in the specified result-type.
			 in n `seq` d `seq` acc'
	 ) (0, 0) foldable

{- |
	* Measures the /dispersion/ of a /population/ of results from the /mean/ value; <https://en.wikipedia.org/wiki/Statistical_dispersion>.

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
	* Determines the exact /variance/ of the specified numbers; <https://en.wikipedia.org/wiki/Variance>.

	* Should the caller define the result-type as 'Rational', then it will be free from rounding-errors.
-}
getVariance :: (
	Data.Foldable.Foldable	foldable,
	Fractional		variance,
	Functor			foldable,
	Real			value
 ) => foldable value -> variance
getVariance	= getDispersionFromMean Math.Power.square

-- | Determines the /standard-deviation/ of the specified numbers; <https://en.wikipedia.org/wiki/Standard_deviation>.
getStandardDeviation :: (
	Data.Foldable.Foldable	foldable,
	Floating		result,
	Functor			foldable,
	Real			value
 ) => foldable value -> result
getStandardDeviation	= sqrt . getVariance

{- |
	* Determines the /average absolute deviation/ of the specified numbers; <https://en.wikipedia.org/wiki/Absolute_deviation#Average_absolute_deviation>.

	* Should the caller define the result-type as 'Rational', then it will be free from rounding-errors.
-}
getAverageAbsoluteDeviation :: (
	Data.Foldable.Foldable	foldable,
	Fractional		result,
	Functor			foldable,
	Real			value
 ) => foldable value -> result
getAverageAbsoluteDeviation	= getDispersionFromMean abs

-- | Determines the /coefficient-of-variance/ of the specified numbers; <https://en.wikipedia.org/wiki/Coefficient_of_variation>.
getCoefficientOfVariance :: (
	Data.Foldable.Foldable	foldable,
	Eq			result,
	Floating		result,
	Functor			foldable,
	Real			value
 ) => foldable value -> result
getCoefficientOfVariance l	= Control.Exception.assert (mean /= 0) $ getStandardDeviation l / abs mean	where
	mean	= getMean l

-- | The number of unordered /combinations/ of /r/ objects taken from /n/; <https://en.wikipedia.org/wiki/Combination>.
nCr :: (Math.Factorial.Algorithmic factorialAlgorithm, Integral i, Show i)
	=> factorialAlgorithm
	-> i	-- ^ The total number of items from which to select.
	-> i	-- ^ The number of items in a sample.
	-> i	-- ^ The number of combinations.
nCr _ 0 _	= 1
nCr _ _ 0	= 1
nCr factorialAlgorithm n r
	| n < r		= 0
	| otherwise	= Control.Exception.assert (n >= 0 && r >= 0) $ numerator `par` (denominator `pseq` numerator `div` denominator)
	where
		[smaller, bigger]	= Data.List.sort [r, n - r]
		numerator		= Math.Implementations.Factorial.risingFactorial (succ bigger) (n - bigger)
		denominator		= Math.Factorial.factorial factorialAlgorithm smaller

-- | The number of /permutations/ of /r/ objects taken from /n/; <https://en.wikipedia.org/wiki/Permutations>.
nPr :: (Integral i, Show i)
	=> i	-- ^ The total number of items from which to select.
	-> i	-- ^ The number of items in a sample.
	-> i	-- ^ The number of permutations.
nPr 0 _	= 1
nPr _ 0	= 1
nPr n r
	| n < r		= 0
	| otherwise	= Control.Exception.assert (n >= 0 && r >= 0) $ Math.Implementations.Factorial.fallingFactorial n r

