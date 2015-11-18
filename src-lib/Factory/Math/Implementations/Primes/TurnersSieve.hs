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

 [@DESCRIPTION@] Generates the constant, conceptally infinite, list of /prime-numbers/, using /Turner's Sieve/; <http://www.haskell.org/haskellwiki/Prime_numbers#Turner.27s_sieve_-_Trial_division>.
-}

module Factory.Math.Implementations.Primes.TurnersSieve(
-- * Functions
	turnersSieve
) where

import qualified	Factory.Math.Power	as Math.Power

{- |
	* For each /prime/, the infinite list of candidates greater than its /square/,
	is filtered for indivisibility; <http://www.haskell.org/haskellwiki/Prime_numbers#Turner.27s_sieve_-_Trial_division>.

	* CAVEAT: though one can easily add a 'Data.PrimeWheel.PrimeWheel', it proved counterproductive.
-}
turnersSieve :: Integral prime => [prime]
turnersSieve	= 2 : sieve [3, 5 ..]	where
	sieve :: Integral i => [i] -> [i]
	sieve []			= []
	sieve (prime : candidates)	= prime : sieve (
		filter (
			\candidate	-> any ($ candidate) [
				(< Math.Power.square prime),	-- Unconditionally admit any candidate smaller than the square of the last prime.
				(/= 0) . (`rem` prime)		-- Ensure indivisibility, of all subsequent candidates, by the last prime discovered.
			]
		) candidates
	 )

