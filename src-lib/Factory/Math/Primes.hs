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

 [@DESCRIPTION@]	Exports a common interface for implementations of /prime-number/ generators.
-}

module Factory.Math.Primes(
-- * Types-classes
	Algorithmic(..),
-- * Functions
	primorial,
	mersenneNumbers
) where

import qualified	Control.DeepSeq
import qualified	Data.Array.IArray

-- | Defines the methods expected of a /prime-number/ generator.
class Algorithmic algorithm	where
	primes	:: (Control.DeepSeq.NFData i, Data.Array.IArray.Ix i, Integral i) => algorithm -> [i]	-- ^ Returns the constant, infinite, list of primes.

{- |
	* Returns the constant list, defining the /Primorial/.

	* <https://en.wikipedia.org/wiki/Primorial>.

	* <http://mathworld.wolfram.com/Primorial.html>.
-}
primorial :: (
	Algorithmic		algorithm,
	Control.DeepSeq.NFData	i,
	Data.Array.IArray.Ix	i,
	Integral		i
 ) => algorithm -> [i]
primorial	= scanl (*) 1 . primes

{- |
	* Returns the constant ordered infinite list of /Mersenne numbers/.

	* Only the subset composed from a prime exponent is returned; which is a strict superset of the /Mersenne Primes/.

	* <https://en.wikipedia.org/wiki/Mersenne_prime>.

	* <http://mathworld.wolfram.com/MersenneNumber.html>
-}
mersenneNumbers :: (Algorithmic algorithm, Integral i) => algorithm -> [i]
mersenneNumbers algorithm	= map (pred . (2 ^)) (primes algorithm :: [Int])	-- Whilst the exponentiation could be parallelised, not all values are known to be required.

