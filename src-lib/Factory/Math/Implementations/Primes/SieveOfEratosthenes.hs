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

	* Generates the constant, conceptually infinite, list of /prime-numbers/, using the /Sieve of Eratosthenes/; <http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes>.

	* Based on <http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf>.

	* The implementation;
		has been optimised using a /wheel/ of static, but parameterised, size;
		is polymorphic, but with a specialisation for type 'Int'.

 [@CAVEAT@] The 'Int'-specialisation is implemented by a /rewrite-rule/, which is /very/ fragile.
-}

module Factory.Math.Implementations.Primes.SieveOfEratosthenes(
-- * Types
-- ** Type-synonyms
--	PrimeMultiplesQueue,
--	PrimeMultiplesMap,
--	Repository,
--	PrimeMultiplesMapInt,
--	RepositoryInt,
-- * Functions
--	head',
--	tail',
	sieveOfEratosthenes,
--	sieveOfEratosthenesInt
) where

import			Control.Arrow((&&&), (***))
import qualified	Control.Arrow
import qualified	Data.IntMap
import qualified	Data.Map
import			Data.Sequence((|>))
import qualified	Data.Sequence
import qualified	Factory.Data.PrimeWheel		as Data.PrimeWheel

-- | The 'Data.Sequence.Seq' counterpart to 'Data.List.head'.
head' :: Data.Sequence.Seq [a] -> [a]
head'	= (`Data.Sequence.index` 0)

{- |
	* The 'Data.Sequence.Seq' counterpart to 'Data.List.tail'.

	* CAVEAT: because @ Data.List.tail [] @ returns an error, whereas @ tail' Data.Sequence.empty @ returns 'Data.Sequence.empty',
	this function is for internal use only.
-}
tail' :: Data.Sequence.Seq [a] -> Data.Sequence.Seq [a]
tail'	= Data.Sequence.drop 1

-- | An ordered queue of the multiples of primes.
type PrimeMultiplesQueue i	= Data.Sequence.Seq (Data.PrimeWheel.PrimeMultiples i)

-- | A map of the multiples of primes.
type PrimeMultiplesMap i	= Data.Map.Map i (Data.PrimeWheel.PrimeMultiples i)

-- | Combine a /queue/, with a /map/, to form a repository to hold prime-multiples.
type Repository i	= (PrimeMultiplesQueue i, PrimeMultiplesMap i)

{- |
	* A refinement of the /Sieve Of Eratosthenes/, which pre-sieves candidates, selecting only those /coprime/ to the specified short sequence of low prime-numbers.

	* The short sequence of initial primes are represented by a 'Data.PrimeWheel.PrimeWheel',
	of parameterised, but static, size; <http://en.wikipedia.org/wiki/Wheel_factorization>.

	* The algorithm requires one to record multiples of previously discovered primes, allowing /composite/ candidates to be eliminated by comparison.

	* Because each /list/ of multiples, starts with the /square/ of the prime from which it was generated,
	the vast majority will be larger than the maximum prime ultimately demanded, and the effort of constructing and storing this list, is consequently wasted.
	Many implementations solve this, by requiring specification of the maximum prime required,
	thus allowing the construction of redundant lists of multiples to be avoided.

	* This implementation doesn't impose that constraint, leaving a requirement for /rapid/ storage,
	which is supported by /appending/ the /list/ of prime-multiples, to a /queue/.
	If a large enough candidate is ever generated, to match the /head/ of the /list/ of prime-multiples,
	at the /head/ of this /queue/, then the whole /list/ of prime-multiples is dropped from the /queue/,
	but the /tail/ of this /list/ of prime-multiples, for which there is now a high likelyhood of a subsequent match, must now be re-recorded.
	A /queue/ doesn't support efficient random /insertion/, so a 'Data.Map.Map' is used for these subsequent multiples.
	This solution is faster than just using a "Data.PQueue.Min".

	* CAVEAT: has linear /O(n)/ space-complexity.
-}
sieveOfEratosthenes :: Integral i
	=> Data.PrimeWheel.NPrimes
	-> [i]
sieveOfEratosthenes	= uncurry (++) . (Data.PrimeWheel.getPrimeComponents &&& start . Data.PrimeWheel.roll) . Data.PrimeWheel.mkPrimeWheel	where
	start :: Integral i => [Data.PrimeWheel.Distance i] -> [i]
	start ~((candidate, rollingWheel) : distances)	= candidate : sieve (head distances) (Data.Sequence.singleton $ Data.PrimeWheel.generateMultiples candidate rollingWheel, Data.Map.empty)

	sieve :: Integral i => Data.PrimeWheel.Distance i -> Repository i -> [i]
	sieve distance@(candidate, rollingWheel) repository@(primeSquares, squareFreePrimeMultiples)	= case Data.Map.lookup candidate squareFreePrimeMultiples of
		Just primeMultiples	-> sieve' $ Control.Arrow.second (insertUniq primeMultiples . Data.Map.delete candidate) repository	-- Re-insert subsequent multiples.
		Nothing -- Not a square-free composite.
			| candidate == smallestPrimeSquare	-> sieve' $ (tail' *** insertUniq subsequentPrimeMultiples) repository	-- Migrate subsequent prime-multiples, from 'primeSquares' to 'squareFreePrimeMultiples'.
			| otherwise {-prime-}			-> candidate : sieve' (Control.Arrow.first (|> Data.PrimeWheel.generateMultiples candidate rollingWheel) repository)
			where
				(smallestPrimeSquare : subsequentPrimeMultiples)	= head' primeSquares
		where
--			sieve' :: Repository i -> [i]
			sieve'	= sieve $ Data.PrimeWheel.rotate distance	-- Tail-recurse.

			insertUniq :: Ord i => Data.PrimeWheel.PrimeMultiples i -> PrimeMultiplesMap i -> PrimeMultiplesMap i
			insertUniq l m	= insert $ dropWhile (`Data.Map.member` m) l	where
--				insert :: Ord i => Data.PrimeWheel.PrimeMultiples i -> PrimeMultiplesMap i
				insert []		= error "Factory.Math.Implementations.Primes.SieveOfEratosthenes.sieveOfEratosthenes.sieve.insertUniq.insert:\tnull list"
				insert (key : values)	= Data.Map.insert key values m

{-# NOINLINE sieveOfEratosthenes #-}
{-# RULES "sieveOfEratosthenes/Int" sieveOfEratosthenes = sieveOfEratosthenesInt #-}	-- CAVEAT: doesn't fire when built with profiling enabled.

-- | A specialisation of 'PrimeMultiplesMap'.
type PrimeMultiplesMapInt	= Data.IntMap.IntMap (Data.PrimeWheel.PrimeMultiples Int)

-- | A specialisation of 'Repository'.
type RepositoryInt	= (PrimeMultiplesQueue Int, PrimeMultiplesMapInt)

{- |
	* A specialisation of 'sieveOfEratosthenes', which approximately /doubles/ the speed and reduces the space required.

	* CAVEAT: because the algorithm involves /squares/ of primes,
	this implementation will overflow when finding primes greater than @2^16@ on a /32-bit/ machine.
-}
sieveOfEratosthenesInt :: Data.PrimeWheel.NPrimes -> [Int]
sieveOfEratosthenesInt	= uncurry (++) . (Data.PrimeWheel.getPrimeComponents &&& start . Data.PrimeWheel.roll) . Data.PrimeWheel.mkPrimeWheel	where
	start :: [Data.PrimeWheel.Distance Int] -> [Int]
	start ~((candidate, rollingWheel) : distances)	= candidate : sieve (head distances) (Data.Sequence.singleton $ Data.PrimeWheel.generateMultiples candidate rollingWheel, Data.IntMap.empty)

	sieve :: Data.PrimeWheel.Distance Int -> RepositoryInt -> [Int]
	sieve distance@(candidate, rollingWheel) repository@(primeSquares, squareFreePrimeMultiples)	= case Data.IntMap.lookup candidate squareFreePrimeMultiples of
		Just primeMultiples	-> sieve' $ Control.Arrow.second (insertUniq primeMultiples . Data.IntMap.delete candidate) repository
		Nothing
			| candidate == smallestPrimeSquare	-> sieve' $ (tail' *** insertUniq subsequentPrimeMultiples) repository
			| otherwise				-> candidate : sieve' (Control.Arrow.first (|> Data.PrimeWheel.generateMultiples candidate rollingWheel) repository)
			where
				(smallestPrimeSquare : subsequentPrimeMultiples)	= head' primeSquares
		where
			sieve' :: RepositoryInt -> [Int]
			sieve'	= sieve $ Data.PrimeWheel.rotate distance

			insertUniq :: Data.PrimeWheel.PrimeMultiples Int -> PrimeMultiplesMapInt -> PrimeMultiplesMapInt
			insertUniq l m	= insert $ dropWhile (`Data.IntMap.member` m) l	where
				insert :: Data.PrimeWheel.PrimeMultiples Int -> PrimeMultiplesMapInt
				insert []		= error "Factory.Math.Implementations.Primes.SieveOfEratosthenes.sieveOfEratosthenesInt.sieve.insertUniq.insert:\tnull list"
				insert (key : values)	= Data.IntMap.insert key values m
