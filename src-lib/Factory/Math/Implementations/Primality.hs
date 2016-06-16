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

	* Determines whether an integer is prime.

	* <https://en.wikipedia.org/wiki/Primality_test>.

	* <http://primes.utm.edu/index.html>

	* CAVEAT: it doesn't determine the prime-factors of composite numbers, just that they exist.
-}

module Factory.Math.Implementations.Primality(
-- * Types
-- ** Data-types
	Algorithm(..)
-- * Functions
-- ** Predicates
--	isPrimeByAKS,
--	isPrimeByMillerRabin,
--	witnessesCompositeness
) where

import			Control.Arrow((&&&))
import qualified	Control.DeepSeq
import qualified	Control.Parallel.Strategies
import qualified	Data.Default
import qualified	Data.Numbers.Primes
import qualified	Factory.Data.MonicPolynomial		as Data.MonicPolynomial
import qualified	Factory.Data.Polynomial			as Data.Polynomial
import qualified	Factory.Data.QuotientRing		as Data.QuotientRing
import qualified	Factory.Math.MultiplicativeOrder	as Math.MultiplicativeOrder
import qualified	Factory.Math.PerfectPower		as Math.PerfectPower
import qualified	Factory.Math.Power			as Math.Power
import qualified	Factory.Math.Primality			as Math.Primality
import qualified	Factory.Math.PrimeFactorisation		as Math.PrimeFactorisation

-- | The algorithms by which /primality/-testing has been implemented.
data Algorithm factorisationAlgorithm
	= AKS factorisationAlgorithm	-- ^ <https://en.wikipedia.org/wiki/AKS_primality_test>.
	| MillerRabin			-- ^ <https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test>.
	deriving (Eq, Read, Show)

instance Data.Default.Default (Algorithm factorisationAlgorithm)	where
	def	= MillerRabin

instance Math.PrimeFactorisation.Algorithmic factorisationAlgorithm => Math.Primality.Algorithmic (Algorithm factorisationAlgorithm)	where
	isPrime _ 2	= True	-- The only even prime.
	isPrime algorithm candidate
		| candidate < 2 || (
			any (
				(== 0) . (candidate `rem`)			-- The candidate has a small prime-factor, and is therefore composite.
			) . filter (
				(candidate >=) . (* 2)				-- The candidate must be at least double the small prime, for it to be a potential factor.
			) . take 5 {-arbitrarily-} $ Data.Numbers.Primes.primes	-- Excludes even numbers, provided at least the 1st prime is tested.
		)		= False
		| otherwise	= (
			case algorithm of
				AKS factorisationAlgorithm	-> isPrimeByAKS factorisationAlgorithm
				MillerRabin			-> isPrimeByMillerRabin
		) candidate

{- |
	* An implementation of the /Agrawal-Kayal-Saxena/ primality-test; <https://en.wikipedia.org/wiki/AKS_primality_test>,
	using the /Lenstra/ and /Pomerance/ algorithm.

	* CAVEAT: this deterministic algorithm has a theoretical time-complexity of @O(log^6)@,
	and therefore can't compete with the performance of probabilistic ones.

	* The /formal polynomials/ used in this algorithm, are conceptually different from /polynomial functions/;
	the /indeterminate/ and its powers, are merely used to name a sequence of pigeon-holes in which /coefficients/ are stored,
	and is never substituted for a specific value.
	This mind-shift, allows one to introduce concepts like /modular/ arithmetic on polynomials,
	which merely represent an operation on their coefficients and the pigeon-hole in which they're placed.

	[@Manindra Agrawal, Neeraj Kayal and Nitin Saxena@]	<http://www.cse.iitk.ac.in/users/manindra/algebra/primality_v6.pdf>.

	[@H. W. Lenstra, Jr. and Carl Pomerance@]		<http://www.math.dartmouth.edu/~carlp/PDF/complexity12.pdf>.

	[@Salembier and Southerington@]				<http://ece.gmu.edu/courses/ECE746/project/F06_Project_resources/Salembier_Southerington_AKS.pdf>,

	[@R. Crandall and J. Papadopoulos@]			<http://images.apple.com/acg/pdf/aks3.pdf>,

	[@Andreas Klappenecker@]				<http://faculty.cs.tamu.edu/klappi/629/aks.ps>,

	[@Vibhor Bhatt and G. K. Patra@]			<http://www.cmmacs.ernet.in/cmmacs/Publications/resch_rep/rrcm0307.pdf>,
-}
isPrimeByAKS :: (
	Control.DeepSeq.NFData			i,
	Integral				i,
	Math.PrimeFactorisation.Algorithmic	factorisationAlgorithm,
	Show					i
 ) => factorisationAlgorithm -> i -> Bool
isPrimeByAKS factorisationAlgorithm n	= and [
	not $ Math.PerfectPower.isPerfectPower n,	-- Step 1.
	Math.Primality.areCoprime n `all` filter (/= n) [2 .. r],	-- Step 3.
	and $ Control.Parallel.Strategies.parMap Control.Parallel.Strategies.rdeepseq	{-Benefits from '+RTS -H100M', which reduces garbage-collections-} (
		\a	-> let
--			lhs, rhs :: Data.Polynomial.Polynomial i i
			lhs	= Data.Polynomial.raiseModulo (Data.Polynomial.mkLinear 1 a) n {-power-} n {-modulus-}
			rhs	= Data.Polynomial.mod' (Data.Polynomial.mkPolynomial [(1, n), (a, 0)]) n
		in Data.QuotientRing.areCongruentModulo (
			Data.MonicPolynomial.mkMonicPolynomial lhs
		) (
			Data.MonicPolynomial.mkMonicPolynomial rhs
		) (
			Data.MonicPolynomial.mkMonicPolynomial modulus
		) -- Because all these polynomials are /monic/, one can establish /congruence/ using /integer/-division.
	) [
		1 .. floor . (* lg) . sqrt $ fromIntegral r
	] -- Step 4; (x + a)^n ~ x^n + a mod (x^r - 1, n).
 ] where
	lg :: Double
	lg	= logBase 2 $ fromIntegral n

--	r :: i
	r	= fst . head . dropWhile (
		(<= floor (Math.Power.square lg)) . snd
	 ) . map (
		id &&& Math.MultiplicativeOrder.multiplicativeOrder factorisationAlgorithm n
	 ) $ Math.Primality.areCoprime n `filter` [2 ..]	-- Step 2.

--	modulus :: Data.Polynomial.Polynomial i i
	modulus	= Data.Polynomial.mkPolynomial [(1, r), (negate 1, 0)]

{- |
	* Uses the specified 'base' in an attempt to prove the /compositeness/ of an integer.

	* This is the opposite of the /Miller Test/; <http://mathworld.wolfram.com/MillersPrimalityTest.html>.

	* If the result is 'True', then the candidate is /composite/; regrettably the converse isn't true.
	Amongst the set of possible bases, over three-quarters are /witnesses/ to the compositeness of a /composite/ candidate,
	the remainder belong to the subset of /liars/.
	In consequence, many false results must be accumulated for different bases, to convincingly identify a prime.
-}
witnessesCompositeness :: (Integral i, Show i)
	=> i	-- ^ Candidate integer.
	-> i
	-> Int
	-> i	-- ^ Base.
	-> Bool
witnessesCompositeness candidate oddRemainder nPowersOfTwo base	= all (
	$ ((`rem` candidate) . Math.Power.square) `iterate` Math.Power.raiseModulo base oddRemainder candidate	-- Repeatedly modulo-square.
 ) [
	(/= 1) . head,					-- Check whether the zeroeth modulo-power is incongruent to one.
	notElem (pred candidate) . take nPowersOfTwo	-- Check whether any modulo-power is incongruent to -1.
 ]

{- |
	* Repeatedly calls 'witnessesCompositeness', to progressively increase the probability of detecting a /composite/ number,
	until ultimately the candidate integer is proven to be prime.

	* Should all bases be tested, then the test is deterministic, but at an efficiency /lower/ than performing prime-factorisation.

	* The test becomes deterministic, for any candidate integer, when the number of tests reaches the limit defined by /Eric Bach/.

	* A testing of smaller set of bases, is sufficient for candidates smaller than various thresholds; <http://primes.utm.edu/prove/prove2_3.html>.

	* <https://en.wikipedia.org/wiki/Miller-Rabin_primality_test>.

	* <http://mathworld.wolfram.com/Rabin-MillerStrongPseudoprimeTest.html>

	* <http://mathworld.wolfram.com/StrongPseudoprime.html>.

	* <http://oeis.org/A014233>, <http://oeis.org/A006945>.
-}
isPrimeByMillerRabin :: (Integral i, Show i) => i -> Bool
isPrimeByMillerRabin primeCandidate	= not $ witnessesCompositeness primeCandidate (
	fst $ last binaryFactors	-- Odd-remainder.
 ) (
	length binaryFactors	-- The number of times that 'two' can be factored-out from 'predecessor'.
 ) `any` testBases	where
	predecessor	= pred primeCandidate
	binaryFactors	= takeWhile ((== 0) . snd) . tail {-drop the original-} $ iterate ((`quotRem` 2) . fst) (predecessor, 0)	-- Factor-out powers of two.
	testBases
		| null fewestPrimeBases	= let
			millersTestSet	= floor . (* 2 {-Eric Bach-}) . Math.Power.square . toRational {-avoid premature rounding-} $ log (fromIntegral primeCandidate :: Double {-overflows at 10^851-})
		in [2 .. predecessor `min` millersTestSet]
		| otherwise		= head fewestPrimeBases `take` Data.Numbers.Primes.primes
		where
			fewestPrimeBases	= map fst $ dropWhile ((primeCandidate >=) . snd) [
				(0,	9),			-- All odd integers less this, are prime, and require no further verification.
				(1,	2047),
				(2,	1373653),
				(3,	25326001),
				(4,	3215031751),
				(5,	2152302898747),		-- Jaeschke ...
				(6,	3474749660383),
				(8,	341550071728321),
				(11,	3825123056546413051),	-- Zhang ...
				(12,	318665857834031151167461),
				(13,	3317044064679887385961981),
				(14,	6003094289670105800312596501),
				(15,	59276361075595573263446330101),
				(17,	564132928021909221014087501701),
				(19,	1543267864443420616877677640751301),
				(20,	10 ^ (36 :: Int))	-- At least.
			 ]

