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

	* Generates the constant list of /prime-numbers/, by a variety of different algorithms.

	* <http://www.haskell.org/haskellwiki/Prime_numbers>.

	* <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.31.3936&rep=rep1&type=pdf>.

	* <http://larc.unt.edu/ian/pubs/sieve.pdf>.
-}

module Factory.Math.Implementations.Primes.Algorithm(
-- * Types
-- ** Data-types
	Algorithm(..)
) where

import qualified	Data.Default
import qualified	Data.Numbers.Primes
import qualified	Factory.Data.PrimeWheel					as Data.PrimeWheel
import qualified	Factory.Math.Implementations.Primes.SieveOfAtkin	as Math.Implementations.Primes.SieveOfAtkin
import qualified	Factory.Math.Implementations.Primes.SieveOfEratosthenes	as Math.Implementations.Primes.SieveOfEratosthenes
import qualified	Factory.Math.Implementations.Primes.TrialDivision	as Math.Implementations.Primes.TrialDivision
import qualified	Factory.Math.Implementations.Primes.TurnersSieve	as Math.Implementations.Primes.TurnersSieve
import qualified	Factory.Math.Primes					as Math.Primes

-- | The implemented methods by which the primes may be generated.
data Algorithm
	= SieveOfAtkin Integer					-- ^ The /Sieve of Atkin/, optimised using a 'Data.PrimeWheel.PrimeWheel' of optimal size, for primes up to the specified maximum bound; <https://en.wikipedia.org/wiki/Sieve_of_Atkin>.
	| SieveOfEratosthenes Data.PrimeWheel.NPrimes		-- ^ The /Sieve of Eratosthenes/ (<https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes>), optimised using a 'Data.PrimeWheel.PrimeWheel'.
	| TrialDivision Data.PrimeWheel.NPrimes			-- ^ For each candidate, confirm indivisibility, by all /primes/ smaller than its /square-root/, optimised using a 'Data.PrimeWheel.PrimeWheel'.
	| TurnersSieve						-- ^ For each /prime/, the infinite list of candidates greater than its /square/, is filtered for indivisibility; <http://www.haskell.org/haskellwiki/Prime_numbers#Turner.27s_sieve_-_Trial_division>.
	| WheelSieve Int					-- ^ 'Data.Numbers.Primes.wheelSieve'.
	deriving (Eq, Read, Show)

instance Data.Default.Default Algorithm	where
	def	= SieveOfEratosthenes 7	-- Resulting in a wheel of circumference 510510.

instance Math.Primes.Algorithmic Algorithm	where
	primes (SieveOfAtkin maxPrime)		= Math.Implementations.Primes.SieveOfAtkin.sieveOfAtkin (Data.PrimeWheel.estimateOptimalSize maxPrime) $ fromIntegral maxPrime
	primes (SieveOfEratosthenes wheelSize)	= Math.Implementations.Primes.SieveOfEratosthenes.sieveOfEratosthenes wheelSize
	primes (TrialDivision wheelSize)	= Math.Implementations.Primes.TrialDivision.trialDivision wheelSize
	primes TurnersSieve			= Math.Implementations.Primes.TurnersSieve.turnersSieve
	primes (WheelSieve wheelSize)		= Data.Numbers.Primes.wheelSieve wheelSize	-- Has better space-complexity than 'SieveOfEratosthenes'.
