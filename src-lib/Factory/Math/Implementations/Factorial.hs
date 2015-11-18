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

 [@DESCRIPTION@]

	* Provides implementations of the class 'Math.Factorial.Algorithmic'.

	* Provides additional functions related to /factorials/, but which depends on a specific implementation,
	and which therefore can't be accessed throught the class-interface.

	* <http://en.wikipedia.org/wiki/Factorial>.

	* <http://mathworld.wolfram.com/Factorial.html>.

	* <http://www.luschny.de/math/factorial/FastFactorialFunctions.htm>.
-}

module Factory.Math.Implementations.Factorial(
-- * Types
-- ** Data-types
	Algorithm(..),
-- * Functions
	primeFactors,
--	primeMultiplicity,
	risingFactorial,
	fallingFactorial,
-- ** Operators
	(!/!)
) where

import qualified	Data.Numbers.Primes
import qualified	Factory.Data.Interval		as Data.Interval
import qualified	Factory.Data.PrimeFactors	as Data.PrimeFactors
import qualified	Factory.Math.Factorial		as Math.Factorial
import qualified	ToolShed.Defaultable

infixl 7 !/!	-- Same as (/).

-- | The algorithms by which /factorial/ has been implemented.
data Algorithm	=
	Bisection		-- ^ The integers from which the /factorial/ is composed, are multiplied using @Data.Interval.product'@.
	| PrimeFactorisation	-- ^ The /prime factors/ of the /factorial/ are extracted, then raised to the appropriate power, before multiplication.
	deriving (Eq, Read, Show)

instance ToolShed.Defaultable.Defaultable Algorithm	where
	defaultValue	= Bisection

instance Math.Factorial.Algorithmic Algorithm	where
	factorial algorithm n
		| n < 2		= 1
		| otherwise	= case algorithm of
			Bisection		-> risingFactorial 2 $ pred n
			PrimeFactorisation	-> Data.PrimeFactors.product' (recip 5) {-empirical-} 10 {-empirical-} $ primeFactors n

{- |
	* Returns the /prime factors/, of the /factorial/ of the specifed integer.

	* Precisely all the primes less than or equal to the specified integer /n/, are included in /n!/;
	only the multiplicity of each of these known prime components need be determined.

	* <http://en.wikipedia.org/wiki/Factorial#Number_theory>.

	* CAVEAT: currently a hotspot.
-}
primeFactors :: Integral base
	=> base					-- ^ The number, whose /factorial/ is to be factorised.
	-> Data.PrimeFactors.Factors base base	-- ^ The /base/ and /exponent/ of each /prime factor/ in the /factorial/, ordered by increasing /base/ (and decreasing /exponent/).
primeFactors n	= takeWhile ((> 0) . snd) $ map (\prime -> (prime, primeMultiplicity prime n)) Data.Numbers.Primes.primes

{- |
	* The number of times a specific /prime/, can be factored from the /factorial/ of the specified integer.

	* General purpose /prime-factorisation/ has /exponential time-complexity/,
	so use /Legendre's Theorem/, which relates only to the /prime factors/ of /factorials/.

	* <http://www.proofwiki.org/wiki/Multiplicity_of_Prime_Factor_in_Factorial>.
-}
primeMultiplicity :: Integral i
	=> i	-- ^ A prime number.
	-> i	-- ^ The integer, the factorial of which the prime is a factor.
	-> i	-- ^ The number of times the prime occurs in the factorial.
primeMultiplicity prime	= sum . takeWhile (> 0) . tail . iterate (`div` prime)

-- | Returns the /rising factorial/; <http://mathworld.wolfram.com/RisingFactorial.html>
risingFactorial :: (Integral i, Show i)
	=> i	-- ^ The lower bound of the integer-range, whose product is returned.
	-> i	-- ^ The number of integers in the range above.
	-> i	-- ^ The result.
risingFactorial _ 0	= 1
risingFactorial 0 _	= 0
risingFactorial x n	= Data.Interval.product' (recip 2) 64 $ Data.Interval.normalise (x, pred $ x + n)

-- | Returns the /falling factorial/; <http://mathworld.wolfram.com/FallingFactorial.html>
fallingFactorial :: (Integral i, Show i)
	=> i	-- ^ The upper bound of the integer-range, whose product is returned.
	-> i	-- ^ The number of integers in the range beneath.
	-> i	-- ^ The result.
fallingFactorial _ 0	= 1
fallingFactorial 0 _	= 0
fallingFactorial x n	= Data.Interval.product' (recip 2) 64 $ Data.Interval.normalise (x, succ $ x - n)

{- |
	* Returns the ratio of two factorials.

	* It is more efficient than evaluating both factorials, and then dividing.

	* For more complex combinations of factorials, such as in the /Binomial coefficient/,
	extract the /prime factors/ using 'primeFactors'
	then manipulate them using the module "Data.PrimeFactors",
	and evaluate it using by /Data.PrimeFactors.product'/.
-}
(!/!) :: (Integral i, Fractional f, Show i)
	=> i	-- ^ The /numerator/.
	-> i	-- ^ The /denominator/.
	-> f	-- ^ The resulting fraction.
numerator !/! denominator
	| numerator <= 1		= recip . fromIntegral $ Math.Factorial.factorial (ToolShed.Defaultable.defaultValue :: Algorithm) denominator
	| denominator <= 1		= fromIntegral $ Math.Factorial.factorial (ToolShed.Defaultable.defaultValue :: Algorithm) numerator
	| numerator == denominator	= 1
	| numerator < denominator	= recip $ denominator !/! numerator	-- Recurse.
	| otherwise			= fromIntegral $ Data.Interval.product' (recip 2) 64 (succ denominator, numerator)

