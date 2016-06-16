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

 [@DESCRIPTION@]	Defines the unit with which precision is measured, and operations on it.
-}
module Factory.Math.Precision(
-- * Types
-- ** Type-synonyms
	ConvergenceOrder,
	ConvergenceRate,
	DecimalDigits,
-- * Constants
	linearConvergence,
	quadraticConvergence,
	cubicConvergence,
	quarticConvergence,
-- * Functions
	getIterationsRequired,
	getTermsRequired,
	roundTo,
	promote,
	simplify
) where

import qualified	Data.Ratio

-- | The /order of convergence/; <https://en.wikipedia.org/wiki/Rate_of_convergence>.
type ConvergenceOrder	= Int

-- | The /rate of convergence/; <https://en.wikipedia.org/wiki/Rate_of_convergence>.
type ConvergenceRate	= Double

-- | A number of decimal digits; presumably positive.
type DecimalDigits	= Int

-- | /Linear/ convergence-rate; which may be qualified by the /rate of convergence/.
linearConvergence :: ConvergenceOrder
linearConvergence	= 1

-- | /Quadratic/ convergence-rate.
quadraticConvergence :: ConvergenceOrder
quadraticConvergence	= 2

-- | /Cubic/ convergence-rate.
cubicConvergence :: ConvergenceOrder
cubicConvergence	= 3

-- | /Quartic/ convergence-rate.
quarticConvergence :: ConvergenceOrder
quarticConvergence	= 4

-- | The predicted number of iterations, required to achieve a specific accuracy, at a given /order of convergence/.
getIterationsRequired :: Integral i
	=> ConvergenceOrder
	-> DecimalDigits	-- ^ The precision of the initial estimate.
	-> DecimalDigits	-- ^ The required precision.
	-> i
getIterationsRequired convergenceOrder initialDecimalDigits requiredDecimalDigits
	| initialDecimalDigits <= 0	= error $ "Factory.Math.Precision.getIterationsRequired:\tinsufficient 'initialDecimalDigits'; " ++ show initialDecimalDigits
	| precisionRatio <= 1		= 0
	| otherwise			= ceiling $ fromIntegral convergenceOrder `logBase` precisionRatio
	where
		precisionRatio :: Double
		precisionRatio	= fromIntegral requiredDecimalDigits / fromIntegral initialDecimalDigits

{- |
	* The predicted number of terms which must be extracted from a series,
	if it is to converge to the required accuracy,
	at the specified linear /convergence-rate/.

	* The /convergence-rate/ of a series, is the error in the series after summation of @(n+1)th@ terms,
	divided by the error after only @n@ terms, as the latter tends to infinity.
	As such, for a /convergent/ series (in which the error get smaller with successive terms), it's value lies in the range @0 .. 1@.

	* <https://en.wikipedia.org/wiki/Rate_of_convergence>.
-}
getTermsRequired :: Integral i
	=> ConvergenceRate
	-> DecimalDigits	-- ^ The additional number of correct decimal digits.
	-> i
getTermsRequired _ 0		= 0
getTermsRequired convergenceRate requiredDecimalDigits
	| convergenceRate <= 0 || convergenceRate >= 1	= error $ "Factory.Math.Precision.getTermsRequired:\t(0 < convergence-rate < 1); " ++ show convergenceRate
	| requiredDecimalDigits < 0			= error $ "Factory.Math.Precision.getTermsRequired:\t'requiredDecimalDigits' must be positive; " ++ show requiredDecimalDigits
	| otherwise					= ceiling $ fromIntegral requiredDecimalDigits / negate (logBase 10 convergenceRate)

-- | Rounds the specified number, to a positive number of 'DecimalDigits'.
roundTo :: (RealFrac a, Fractional f) => DecimalDigits -> a -> f
roundTo decimals = (/ fromInteger promotionFactor) . fromInteger . round . (* fromInteger promotionFactor)	where
	promotionFactor :: Integer
	promotionFactor	= 10 ^ decimals

-- | Promotes the specified number, by a positive number of 'DecimalDigits'.
promote :: Num n => n -> DecimalDigits -> n
promote x	= (* x) . (10 ^)

{- |
	* Reduces a 'Rational' to the minimal form required for the specified number of /fractional/ decimal places;
	irrespective of the number of integral decimal places.

	* A 'Rational' approximation to an irrational number, may be very long, and provide an unknown excess precision.
	Whilst this doesn't sound harmful, it costs in performance and memory-requirement, and being unpredictable isn't actually useful.
-}
simplify :: RealFrac operand
	=> DecimalDigits	-- ^ The number of places after the decimal point, which are required.
	-> operand
	-> Rational
simplify decimalDigits operand	= Data.Ratio.approxRational operand . recip $ 4 * 10 ^ succ decimalDigits	-- Tolerate any error less than half the least significant digit required.

