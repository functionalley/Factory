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

 [@DESCRIPTION@]	Generates the constant, conceptually infinite, list of /prime-numbers/, using /Trial Division/.
-}

module Factory.Math.Implementations.Primes.TrialDivision(
-- * Functions
	trialDivision
-- ** Predicates
--	isIndivisibleBy
) where

import qualified	Control.Arrow
import qualified	Data.List
import qualified	Factory.Math.Power		as Math.Power
import qualified	Factory.Math.PrimeFactorisation	as Math.PrimeFactorisation
import qualified	Factory.Data.PrimeWheel		as Data.PrimeWheel

-- | Uses /Trial Division/, to determine whether the specified candidate is indivisible by all potential denominators from the specified list.
isIndivisibleBy :: Integral i
	=> i	-- ^ The numerator.
	-> [i]	-- ^ The denominators of which it must not be a multiple.
	-> Bool
isIndivisibleBy numerator	= all ((/= 0) . (numerator `rem`)) . takeWhile (<= Math.PrimeFactorisation.maxBoundPrimeFactor numerator)

{- |
	* For each candidate, confirm indivisibility, by all /primes/ smaller than its /square-root/.

	* The candidates to sieve, are generated by a 'Data.PrimeWheel.PrimeWheel',
	of parameterised, but static, size; <http://en.wikipedia.org/wiki/Wheel_factorization>.
-}
trialDivision :: Integral prime => Data.PrimeWheel.NPrimes -> [prime]
trialDivision 0	= [2, 3] ++ filter (`isIndivisibleBy` trialDivision 0 {-recurse-}) [5 ..]	-- No faster than using 'Data.PrimeWheel.mkPrimeWheel 0', but apparently better space-complexity ?!
trialDivision wheelSize	= Data.PrimeWheel.getPrimeComponents primeWheel ++ indivisible	where
	primeWheel	= Data.PrimeWheel.mkPrimeWheel wheelSize
	candidates	= map fst $ Data.PrimeWheel.roll primeWheel
	indivisible	= uncurry (++) . Control.Arrow.second (
		filter (`isIndivisibleBy` indivisible {-recurse-})
	 ) $ Data.List.span (
		< Math.Power.square (head candidates)	-- The first composite candidate, is the square of the next prime after the wheel's constituent ones.
	 ) candidates
