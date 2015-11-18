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

	* Generates the constant /bounded/ list of /prime-numbers/, using the /Sieve of Atkin/; <http://en.wikipedia.org/wiki/Sieve_of_Atkin>.

	* <cr.yp.to/papers/primesieves-19990826.pdf>.

	* The implementation;
		has been optimised using a /wheel/ of static, but parameterised, size;
		has been parallelized;
		is polymorphic, but with a specialisation for type 'Int'.

 [@CAVEAT@] The 'Int'-specialisation is implemented by a /rewrite-rule/, which is /very/ fragile.
-}

module Factory.Math.Implementations.Primes.SieveOfAtkin(
-- * Types
-- ** Data-types
--	PolynomialType,
-- * Constants
--	atkinsModulus,
--	inherentPrimes,
--	nInherentPrimes,
--	squares,
-- * Functions
--	polynomialTypeLookupPeriod,
--	polynomialTypeLookup,
--	findPolynomialSolutions,
--	filterOddRepetitions,
--	generateMultiplesOfSquareTo,
--	getPrefactoredPrimes,
	sieveOfAtkin,
--	sieveOfAtkinInt
) where

import qualified	Control.DeepSeq
import qualified	Control.Parallel.Strategies
import qualified	Data.Array.IArray
import			Data.Array.IArray((!))
import qualified	Data.IntSet
import qualified	Data.List
import qualified	Data.Set
import qualified	Factory.Data.PrimeWheel	as Data.PrimeWheel
import qualified	Factory.Math.Power	as Math.Power
import qualified	ToolShed.Data.List

-- | Defines the types of /quadratic/, available to test the potential primality of a candidate integer.
data PolynomialType
	= ModFour	-- ^ Suitable for primality-testing numbers meeting @(n `mod` 4 == 1)@.
	| ModSix	-- ^ Suitable for primality-testing numbers meeting @(n `mod` 6 == 1)@.
	| ModTwelve	-- ^ Suitable for primality-testing numbers meeting @(n `mod` 12 == 11)@.
	| None		-- ^ There's no polynomial which can assess primality, because the candidate is composite.
	deriving Eq

-- | The constant modulus used to select the appropriate quadratic for a prime candidate.
atkinsModulus :: Integral i => i
atkinsModulus	= foldr1 lcm [4, 6, 12]	-- Sure, this is always '12', but this is the reason why.

-- | The constant list of primes factored-out by the unoptimised algorithm.
inherentPrimes :: Integral i => [i]
inherentPrimes	= [2, 3]

-- | The constant number of primes factored-out by the unoptimised algorithm.
nInherentPrimes :: Int
nInherentPrimes	= length (inherentPrimes :: [Int])

-- | Typically the set of primes which have been built into the specified /wheel/, but never fewer than 'inherentPrimes'.
getPrefactoredPrimes :: Integral i => Data.PrimeWheel.PrimeWheel i -> [i]
getPrefactoredPrimes	= max inherentPrimes . Data.PrimeWheel.getPrimeComponents

-- | The period over which the data returned by 'polynomialTypeLookup' repeats.
polynomialTypeLookupPeriod :: Integral i => Data.PrimeWheel.PrimeWheel i -> i
polynomialTypeLookupPeriod	= lcm atkinsModulus . Data.PrimeWheel.getCircumference

{- |
	* Defines which, if any, of the three /quadratics/ is appropriate for the primality-test for each candidate.

	* Since this algorithm uses /modular arithmetic/, the /range/ of results repeat after a short /domain/ related to the /modulus/.
	Thus one need calculate at most one period of this cycle, but fewer if the maximum prime required falls within the first cycle of results.

	* Because the results are /bounded/, they're returned in a zero-indexed /array/, to provide efficient random access;
	the first few elements should never be required, but it makes query clearer.

	* <http://en.wikipedia.org/wiki/Sieve_of_Atkin>.
-}
polynomialTypeLookup :: (Data.Array.IArray.Ix i, Integral i)
	=> Data.PrimeWheel.PrimeWheel i
	-> i	-- ^ The maximum prime required.
	-> Data.Array.IArray.Array i PolynomialType
polynomialTypeLookup primeWheel maxPrime	= Data.Array.IArray.listArray (0, pred (polynomialTypeLookupPeriod primeWheel) `min` maxPrime) $ map select [0 ..]	where
--	select :: Integral i => i -> PolynomialType
	select n
		| any (
			(== 0) . (n `rem`)		-- Though this is merely /Trial Division/, it's only performed over a short bounded interval of numerators.
		) primeComponents	= None
		| r `elem` [1, 5]	= ModFour	-- We actually require @(n `mod` 4 == 1)@, but this is the equivalent modulo 12, with @(r == 9)@ removed because they're all divisible by /3/.
		| r == 7		= ModSix	-- We actually require @(n `mod` 6 == 1)@, but this is the equivalent modulo 12, where @(r == 1)@ has been accounted for above.
		| r == 11		= ModTwelve	-- We require @(n `mod` 12 == 11)@.
		| otherwise		= None
		where
			r		= n `rem` atkinsModulus
			primeComponents	= drop nInherentPrimes $ Data.PrimeWheel.getPrimeComponents primeWheel

-- | The constant, infinite list of the /squares/, of integers increasing from /1/.
squares :: Integral i => [i]
squares	= map snd $ Math.Power.squaresFrom 1

{- |
	* Returns the /ordered/ list of those values with an /odd/ number of occurrences in the specified /unordered/ list.

	* CAVEAT: this is expensive in both execution-time and space.
	The typical imperative-style implementation accumulates polynomial-solutions in a /mutable array/ indexed by the candidate integer.
	This doesn't translate seamlessly to the /pure functional/ domain where /arrays/ naturally immutable,
	so we /sort/ a /list/ of polynomial-solutions, then measure the length of the solution-spans, corresponding to viable candidates.
	Regrettably, 'Data.List.sort' (implemented in /GHC/ by /mergesort/) has a time-complexity /O(n*log n)/
	which is greater than the theoretical /O(n)/ of the whole /Sieve of Atkin/;
	/GHC/'s old /qsort/-implementation is even slower :(
-}
filterOddRepetitions :: Ord a => [a] -> [a]
-- filterOddRepetitions	= map head . filter (foldr (const not) False) . Data.List.group . Data.List.sort	-- Too slow.
filterOddRepetitions	= slave True . Data.List.sort where
	slave isOdd (one : remainder@(two : _))
		| one == two	= slave (not isOdd) remainder
		| isOdd		= one : beginSpan
		| otherwise	= beginSpan
		where
			beginSpan	= slave True remainder
	slave True [singleton]	= [singleton]
	slave _ _		= []

{- |
	* Returns the ordered list of solutions aggregated from each of three /bivariate quadratics/; @z = f(x, y)@.

	* For a candidate integer to be prime, it is necessary but insufficient, that there are an /odd/ number of solutions of value /candidate/.

	* At most one of these three polynomials is suitable for the validation of any specific candidate /z/, depending on 'lookupPolynomialType'.
	so the three sets of solutions are mutually exclusive.
	One coordinate @(x, y)@, can have solutions in more than one of the three polynomials.

	* This algorithm exhaustively traverses the domain @(x, y)@, for resulting /z/ of the required modulus.
	Whilst it tightly constrains the bounds of the search-space, it searches the domain methodically rather than intelligently.
-}
findPolynomialSolutions :: (Control.DeepSeq.NFData i, Data.Array.IArray.Ix i, Integral i)
	=> Data.PrimeWheel.PrimeWheel i
	-> i	-- ^ The maximum prime-number required.
	-> [i]
findPolynomialSolutions primeWheel maxPrime	= foldr1 ToolShed.Data.List.merge {-The lists were previously sorted, as a side-effect, by 'filterOddRepetitions'-} $ Control.Parallel.Strategies.withStrategy (
		Control.Parallel.Strategies.parList Control.Parallel.Strategies.rdeepseq
	 ) [
		{-# SCC "4x^2+y^2" #-} filterOddRepetitions [
			z |
				x'	<- takeWhile (<= pred maxPrime) $ map (* 4) squares,
				z	<- takeWhile (<= maxPrime) $ map (+ x') oddSquares,
				lookupPolynomialType z == ModFour
		], -- List-comprehension. Twice the length of the other two lists.
		{-# SCC "3x^2+y^2" #-} filterOddRepetitions [
			z |
				x'	<- takeWhile (<= pred maxPrime) $ map (* 3) squares,
				z	<- takeWhile (<= maxPrime) . map (+ x') $ if even x' then oddSelection else evenSelection,
				lookupPolynomialType z == ModSix
		], -- List-comprehension.
		{-# SCC "3x^2-y^2" #-} filterOddRepetitions [
			z |
				x2	<- takeWhile (<= maxPrime `div` 2) squares,
				z	<- dropWhile (> maxPrime) . map (3 * x2 -) . takeWhile (< x2) $ if even x2 then oddSelection else evenSelection,
				lookupPolynomialType z == ModTwelve
		] -- List-comprehension.
	] where
		(evenSquares, oddSquares)	= Data.List.partition even squares

--		evenSelection, oddSelection :: Integral i => [i]
		evenSelection	= selection110 evenSquares	where
			selection110 (x0 : x1 : _ : xs)	= x0 : x1 : selection110 xs	-- Effectively, those for meeting ((== 4) . (`mod` 6)).
			selection110 xs			= xs
		oddSelection	= selection101 oddSquares	where
			selection101 (x0 : _ : x2 : xs)	= x0 : x2 : selection101 xs	-- Effectively, those for meeting ((== 1) . (`mod` 6)).
			selection101 xs			= xs

--		lookupPolynomialType :: (Data.Array.IArray.Ix i, Integral i) => i -> PolynomialType
		lookupPolynomialType	= (polynomialTypeLookup primeWheel maxPrime !) . (`rem` polynomialTypeLookupPeriod primeWheel)

-- | Generates the /bounded/ list of multiples, of the /square/ of the specified prime, skipping those which aren't required.
generateMultiplesOfSquareTo :: Integral i
	=> Data.PrimeWheel.PrimeWheel i	-- ^ Used to generate the gaps between prime multiples of the square.
	-> i				-- ^ The /prime/.
	-> i				-- ^ The maximum bound.
	-> [i]
generateMultiplesOfSquareTo primeWheel prime max'	= takeWhile (<= max') . scanl (\accumulator -> (+ accumulator) . (* prime2)) prime2 . cycle $ Data.PrimeWheel.getSpokeGaps primeWheel	where
	prime2	= Math.Power.square prime

{- |
	* Generates the constant /bounded/ list of /prime-numbers/.

	* <http://cr.yp.to/papers/primesieves-19990826.pdf>
-}
sieveOfAtkin :: (Control.DeepSeq.NFData i, Data.Array.IArray.Ix i, Integral i)
	=> Data.PrimeWheel.NPrimes	-- ^ Other implementations effectively use a hard-coded value either /2/ or /3/, but /6/ seems better.
	-> i				-- ^ The maximum prime required.
	-> [i]				-- ^ The /bounded/ list of primes.
sieveOfAtkin wheelSize maxPrime	= (prefactoredPrimes ++) . filterSquareFree Data.Set.empty . dropWhile (<= maximum prefactoredPrimes) $ findPolynomialSolutions primeWheel maxPrime	where
	primeWheel		= Data.PrimeWheel.mkPrimeWheel wheelSize
	prefactoredPrimes	= getPrefactoredPrimes primeWheel

--	filterSquareFree :: Integral i => Data.Set.Set i -> [i] -> [i]
	filterSquareFree _ []	= []
	filterSquareFree primeMultiples (candidate : candidates)
		| Data.Set.member candidate primeMultiples	= {-# SCC "delete" #-} filterSquareFree (Data.Set.delete candidate primeMultiples) candidates	-- Tail-recurse.
		| otherwise					= {-# SCC "insert" #-} candidate : filterSquareFree (Data.Set.union primeMultiples . Data.Set.fromDistinctAscList $ generateMultiplesOfSquareTo primeWheel candidate maxPrime) candidates

{-# NOINLINE sieveOfAtkin #-}
{-# RULES "sieveOfAtkin/Int" sieveOfAtkin = sieveOfAtkinInt #-}	-- CAVEAT: doesn't fire when built with profiling enabled.

-- | A specialisation of 'sieveOfAtkin', which reduces both the execution-time and the space required.
sieveOfAtkinInt :: Data.PrimeWheel.NPrimes -> Int -> [Int]
sieveOfAtkinInt wheelSize maxPrime	= (prefactoredPrimes ++) . filterSquareFree Data.IntSet.empty . dropWhile (<= maximum prefactoredPrimes) $ findPolynomialSolutions primeWheel maxPrime	where
	primeWheel		= Data.PrimeWheel.mkPrimeWheel wheelSize
	prefactoredPrimes	= getPrefactoredPrimes primeWheel

	filterSquareFree :: Data.IntSet.IntSet -> [Int] -> [Int]
	filterSquareFree _ []	= []
	filterSquareFree primeMultiples (candidate : candidates)
		| Data.IntSet.member candidate primeMultiples	= filterSquareFree (Data.IntSet.delete candidate primeMultiples) candidates
		| otherwise					= candidate : filterSquareFree (Data.IntSet.union primeMultiples . Data.IntSet.fromDistinctAscList $ generateMultiplesOfSquareTo primeWheel candidate maxPrime) candidates

