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

 [@DESCRIPTION@]	Defines a /prime-wheel/, for use in prime-number generation; <https://en.wikipedia.org/wiki/Wheel_factorization>.
-}

module Factory.Data.PrimeWheel(
-- * Types
-- ** Type-synonyms
	Distance,
	NPrimes,
	PrimeMultiples,
--	Repository,
-- ** Data-types
	PrimeWheel(getPrimeComponents, getSpokeGaps),
-- * Functions
	estimateOptimalSize,
--	findCoprimes,
	generateMultiples,
	roll,
	rotate,
-- ** Constructors
	mkPrimeWheel,
-- ** Query
	getCircumference,
	getSpokeCount
) where

import			Control.Arrow((&&&), (***))
import qualified	Data.IntMap
import qualified	Data.List

{- |
	* A conceptual /wheel/, with irregularly spaced spokes; <http://www.haskell.org/haskellwiki/Prime_numbers_miscellaneous#Prime_Wheels>.

	* On being rolled, the trace of the spokes, identifies candidates which are /coprime/ to those primes from which the /wheel/ was composed.

	* One can alternatively view this as a set of vertical nested rings, each with a /prime circumference/, and touching at its lowest point.
	Each has a single mark on its /circumference/, which when rolled identifies multiples of that /circumference/.
	When the complete set is rolled, from the state where all marks are coincident, all multiples of the set of primes, are traced.

	* CAVEAT: The distance required to return to this state (the wheel's /circumference/), grows rapidly with the number of primes:

>	zip [0 ..] . scanl (*) 1 $ [2,3,5,7,11,13,17,19,23,29,31]
>	[(0,1),(1,2),(2,6),(3,30),(4,210),(5,2310),(6,30030),(7,510510),(8,9699690),(9,223092870),(10,6469693230),(11,200560490130)]

	* The number of spokes also grows rapidly with the number of primes:

>	zip [0 ..] . scanl (*) 1 . map pred $ [2,3,5,7,11,13,17,19,23,29,31]
>	[(0,1),(1,1),(2,2),(3,8),(4,48),(5,480),(6,5760),(7,92160),(8,1658880),(9,36495360),(10,1021870080),(11,30656102400)]
-}
data PrimeWheel i	= MkPrimeWheel {
	getPrimeComponents	:: [i],	-- ^ Accessor: the ordered sequence of initial primes, from which the /wheel/ was composed.
	getSpokeGaps		:: [i]	-- ^ Accessor: the sequence of spoke-gaps, the sum of which equals its /circumference/.
} deriving Show

-- | The /circumference/ of the specified 'PrimeWheel'.
getCircumference :: Integral i => PrimeWheel i -> i
getCircumference	= product . getPrimeComponents

-- | The number of spokes in the specified 'PrimeWheel'.
getSpokeCount :: Integral i => PrimeWheel i -> i
getSpokeCount	= foldr ((*) . pred) 1 . getPrimeComponents

-- | An infinite increasing sequence, of the multiples of a specific prime.
type PrimeMultiples i	= [i]

-- | Defines a container for the 'PrimeMultiples'.
type Repository	= Data.IntMap.IntMap (PrimeMultiples Int)

-- | The size of the /wheel/, measured by the number of primes from which it is composed.
type NPrimes	= Int

{- |
	* Uses a /Sieve of Eratosthenes/ (<https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes>), to generate an initial sequence of primes.

	* Also generates an infinite sequence of candidate primes, each of which is /coprime/ to the primes just found, e.g.:
	@filter ((== 1) . (gcd (2 * 3 * 5 * 7))) [11 ..] = [11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,121 ..]@; NB /121/ isn't prime.

	* CAVEAT: the use, for efficiency, of "Data.IntMap", limits the maximum bound of this sequence, though not to a significant extent.
-}
findCoprimes :: NPrimes -> ([Int], [Int])
findCoprimes 0	= ([], [])
findCoprimes required
	| required < 0	= error $ "Factory.Data.PrimeWheel.findCoprimes: invalid number of coprimes; " ++ show required
	| otherwise	= splitAt required $ 2 : sieve 3 0 Data.IntMap.empty
	where
		sieve :: Int -> NPrimes -> Repository -> [Int]
		sieve candidate found repository	= case Data.IntMap.lookup candidate repository of
			Just primeMultiples	-> sieve' found . insertUniq primeMultiples $ Data.IntMap.delete candidate repository	-- Re-insert subsequent multiples.
			Nothing {-prime-}	-> let
				found'		= succ found
				(key : values)	= iterate (+ gap * candidate) $ candidate ^ (2 :: Int)	-- Generate a sequence of prime-multiples, starting from its square.
			 in candidate : sieve' found' (
				if found' >= required
					then repository
					else Data.IntMap.insert key values repository
			 )
			where
				gap :: Int
				gap	= 2	-- For efficiency, only sieve odd integers.

				sieve' :: NPrimes -> Repository -> [Int]
				sieve'	= sieve $ candidate + gap	-- Tail-recurse.

				insertUniq :: PrimeMultiples Int -> Repository -> Repository
				insertUniq l m	= insert $ dropWhile (`Data.IntMap.member` m) l	where
					insert :: PrimeMultiples Int -> Repository
					insert []		= error "Factory.Data.PrimeWheel.findCoprimes.sieve.insertUniq.insert:\tnull list"
					insert (key : values)	= Data.IntMap.insert key values m
{- |
	* The optimal number of low primes from which to build the /wheel/, grows with the number of primes required;
	the /circumference/ should be approximately the /square-root/ of the number of integers it will be required to sieve.

	* CAVEAT: one greater than this is returned, which empirically seems better.
-}
estimateOptimalSize :: Integral i => i -> NPrimes
estimateOptimalSize maxPrime	= succ . length . takeWhile (<= optimalCircumference) . scanl1 (*) {-circumference-} . map fromIntegral {-prevent overflow-} . fst {-primes-} $ findCoprimes 10 {-arbitrary maximum bound-}	where
	optimalCircumference :: Integer
	optimalCircumference	= round (sqrt $ fromIntegral maxPrime :: Double)

{- |
	Smart constructor for a /wheel/ from the specified number of low primes.

	* The optimal number of low primes from which to build the /wheel/, grows with the number of primes required;
	the /circumference/ should be approximately the /square-root/ of the number of integers it will be required to sieve.

	* The sequence of gaps between spokes on the /wheel/ is /symmetrical under reflection/;
	though two values lie /on/ the axis, that aren't part of this symmetry. Eg:

>	nPrimes	Gaps
>	======	====
>	0	[1]
>	1	[2]	-- The terminal gap for all subsequent wheels is '2'; [(succ circumference `mod` circumference) - (pred circumference `mod` circumference)].
>	2	[4,2]	-- Both points are on the axis, so the symmetry isn't yet clear.
>	3	[6,4,2,4,2,4,6,2]
>	4	[10,2,4,2,4,6,2,6,4,2,4,6,6,2,6,4,2,6,4,6,8,4,2,4,2,4,8,6,4,6,2,4,6,2,6,6,4,2,4,6,2,6,4,2,4,2,10,2]

	Exploitation of this property has proved counter-productive, probably because it requires /strict evaluation/,
	exposing the user to the full cost of inadvertently choosing a /wheel/, which in practice, is rotated less than once.
-}
mkPrimeWheel :: Integral i => NPrimes -> PrimeWheel i
mkPrimeWheel 0	= MkPrimeWheel [] [1]
mkPrimeWheel nPrimes
	| nPrimes < 0	= error $ "Factory.Data.PrimeWheel.mkPrimeWheel: unable to construct from " ++ show nPrimes ++ " primes"
	| otherwise	= primeWheel
	where
		(primeComponents, coprimeCandidates)	= (map fromIntegral *** map fromIntegral . Data.List.genericTake (getSpokeCount primeWheel)) $ findCoprimes nPrimes
		primeWheel				= MkPrimeWheel primeComponents $ zipWith (-) coprimeCandidates $ 1 : coprimeCandidates	-- Measure the gaps between candidate primes.

-- | Couples a candidate prime with a /rolling wheel/, to define the distance rolled.
type Distance i	= (i, [i])

-- | Generates a new candidate prime, from a /rolling wheel/, and the current candidate.
rotate :: Integral i => Distance i -> Distance i
rotate (candidate, rollingWheel)	= (candidate +) . head &&& tail $ rollingWheel

{-# INLINE rotate #-}

-- | Generate an infinite, increasing sequence of candidate primes, from the specified /wheel/.
roll :: Integral i => PrimeWheel i -> [Distance i]
roll primeWheel	= tail $ iterate rotate (1, cycle $ getSpokeGaps primeWheel)

{- |
	* Generates multiples of the specified prime, starting from its /square/,
	skipping those multiples of the low primes from which the specified 'PrimeWheel' was composed,
	and which therefore, the /wheel/ won't generate as candidates. Eg:

>	Prime	Rotating PrimeWheel 3	Output
>	=====	=====================	======
>	7	[4,2,4,2,4,6,2,6]	[49,77,91,119,133,161,203,217,259 ..]
>	11	[2,4,2,4,6,2,6,4]	[121,143,187,209,253,319,341,407 ..]
>	13	[4,2,4,6,2,6,4,2]	[169,221,247,299,377,403,481,533,559 ..]
-}
generateMultiples :: Integral i
	=> i	-- ^ The number to square and multiply
	-> [i]	-- ^ A /rolling wheel/, the track of which, delimits the gaps between /coprime/ candidates.
	-> [i]
generateMultiples i	= scanl (\accumulator -> (+ accumulator) . (* i)) (i ^ (2 :: Int))

{-# INLINE generateMultiples #-}

