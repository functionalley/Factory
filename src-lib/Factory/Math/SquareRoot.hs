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

 [@DESCRIPTION@]

	* Exports a common interface for /square-root/ implementations.

	* Provides utilities for these implementations.
-}

module Factory.Math.SquareRoot(
-- * Type-classes
	Algorithmic(..),
	Iterator(..),
-- * Types
-- ** Type-synonyms
	Result,
	Estimate,
-- * Functions
	getAccuracy,
	getDiscrepancy,
	getEstimate,
--	rSqrt,
-- ** Predicates
	isPrecise
) where

import qualified	Factory.Math.Power	as Math.Power
import qualified	Factory.Math.Precision	as Math.Precision

-- | The result-type; actually, only the concrete return-type of 'Math.Precision.simplify', stops it being a polymorphic instance of 'Fractional'.
type Result	= Rational

-- | Contains an estimate for the /square-root/ of a value, and its accuracy.
type Estimate	= (Result, Math.Precision.DecimalDigits)

-- | Defines the methods expected of a /square-root/ algorithm.
class Algorithmic algorithm	where
	squareRootFrom	:: (Real operand, Show operand)
		=> algorithm
		-> Estimate			-- ^ An initial estimate from which to start.
		-> Math.Precision.DecimalDigits	-- ^ The required precision.
		-> operand			-- ^ The value for which to find the /square-root/.
		-> Result			-- ^ Returns an improved estimate of the /square-root/, found using the specified algorithm, accurate to at least the required number of decimal digits.

	squareRoot	:: (Real operand, Show operand)
		=> algorithm
		-> Math.Precision.DecimalDigits	-- ^ The required precision.
		-> operand			-- ^ The value for which to find the /square-root/.
		-> Result			-- ^ Returns an estimate of the /square-root/, found using the specified algorithm, accurate to at least the required number of decimal digits.
	squareRoot algorithm decimalDigits operand	= squareRootFrom algorithm (getEstimate operand) decimalDigits operand	-- Default implementation

-- | The interface required to iterate, from an estimate of the required value, to the next approximation.
class Iterator algorithm where
	step :: Real operand
		=> algorithm
		-> operand	-- ^ The value for which the /square-root/ is required; @y@.
		-> Result	-- ^ The current estimate; @x(n)@.
		-> Result	-- ^ An improved estimate; @x(n+1)@.

	convergenceOrder :: algorithm -> Math.Precision.ConvergenceOrder	-- ^ The ultimate ratio of successive terms as the iteration converges.

-- | Generalise 'sqrt' to operate on any 'Real' operand.
rSqrt :: Real operand => operand -> Double
rSqrt	= sqrt . realToFrac

-- | Uses 'Double'-precision floating-point arithmetic, to obtain an initial estimate for the /square-root/, and its accuracy.
getEstimate :: (Real operand, Show operand) => operand -> Estimate
getEstimate y
	| y < 0		= error $ "Factory.Math.SquareRoot.getEstimate:\tthere's no real square-root of " ++ show y
	| otherwise	= (Math.Precision.simplify decimalDigits {-doubles performance by roughly length of the Rational representation-} . toRational $ rSqrt y, decimalDigits)
	where
		decimalDigits :: Math.Precision.DecimalDigits
		decimalDigits	= 16	-- <https://en.wikipedia.org/wiki/IEEE_floating_point>.

{- |
	* The signed difference between the /square/ of an estimate for the /square-root/ of a value, and that value.

	* Positive when the estimate is too low.

	* CAVEAT: the magnitude is twice the error in the /square-root/.
-}
getDiscrepancy :: Real operand => operand -> Result -> Result
getDiscrepancy y x	= toRational y - Math.Power.square x

-- | True if the specified estimate for the /square-root/, is precise.
isPrecise :: Real operand => operand -> Result -> Bool
isPrecise y x	= getDiscrepancy y x == 0

{- |
	* For a given value and an estimate of its /square-root/,
	returns the number of decimals digits to which the /square-root/ is accurate; including the integral digits.

	* CAVEAT: the result returned for an exact match has been bodged.
-}
getAccuracy :: Real operand => operand -> Result -> Math.Precision.DecimalDigits
getAccuracy y x
	| absoluteError == 0	= maxBound	-- Bodge.
--	| otherwise		= length . takeWhile (< 1) $ iterate (* 10) relativeError	-- CAVEAT: too slow.
	| otherwise		= length $ show (round $ toRational y / absoluteError :: Integer)
	where
		absoluteError :: Result
		absoluteError	= abs (getDiscrepancy y x) / 2	-- N.B.: the magnitude of the error in 'y', is twice the error in its square-root, 'x'.

