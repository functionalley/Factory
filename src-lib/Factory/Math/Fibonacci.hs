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

 [@DESCRIPTION@]	<https://en.wikipedia.org/wiki/Fibonacci_number>.
-}

module Factory.Math.Fibonacci(
-- * Constants
	fibonacci,
	primeIndexedFibonacci
) where

import qualified	Data.Numbers.Primes

-- | A constant ordered list of the /Fibonacci/-numbers.
fibonacci :: Integral i => [i]
fibonacci	= 0 : scanl (+) 1 fibonacci

{- |
	* The subset of 'fibonacci', /indexed/ by a /prime/-number.

	* <http://primes.utm.edu/glossary/page.php?sort=FibonacciPrime>.
-}
primeIndexedFibonacci :: Integral i => [i]
primeIndexedFibonacci	= map (fibonacci !!) Data.Numbers.Primes.primes

