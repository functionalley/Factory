{-
	Copyright (C) 2011-2017 Dr. Alistair Ward

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

	* Implements several different prime-factorisation algorithms.

	* <http://www.tug.org/texinfohtml/coreutils.html#factor-invocation>.
-}

module Factory.Math.Implementations.PrimeFactorisation(
-- * Types
-- ** Data-types
	Algorithm(
--		DixonsMethod,
		FermatsMethod,
		TrialDivision
	)
-- * Functions
--	factoriseByDixonsMethod
--	factoriseByFermatsMethod
--	factoriseByTrialDivision
) where

import			Control.Arrow((&&&))
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Control.Parallel.Strategies
import qualified	Data.Default
import qualified	Data.Maybe
import qualified	Data.Numbers.Primes
import qualified	Factory.Data.Exponential	as Data.Exponential
import			Factory.Data.Exponential((<^))
import qualified	Factory.Data.PrimeFactors	as Data.PrimeFactors
import qualified	Factory.Math.PerfectPower	as Math.PerfectPower
import qualified	Factory.Math.Power		as Math.Power
import qualified	Factory.Math.PrimeFactorisation	as Math.PrimeFactorisation
import qualified	ToolShed.Data.Pair

-- | The algorithms by which prime-factorisation has been implemented.
data Algorithm
	= DixonsMethod	-- ^ <https://en.wikipedia.org/wiki/Dixon%27s_factorization_method>.
	| FermatsMethod	-- ^ <https://en.wikipedia.org/wiki/Fermat%27s_factorization_method>.
	| TrialDivision	-- ^ <https://en.wikipedia.org/wiki/Trial_division>.
	deriving (Eq, Read, Show)

instance Data.Default.Default Algorithm	where
	def	= TrialDivision

instance Math.PrimeFactorisation.Algorithmic Algorithm	where
	primeFactors algorithm	= case algorithm of
		DixonsMethod	-> factoriseByDixonsMethod
		FermatsMethod	-> Data.PrimeFactors.reduce . factoriseByFermatsMethod
		TrialDivision	-> factoriseByTrialDivision

-- | <https://en.wikipedia.org/wiki/Dixon%27s_factorization_method>.
factoriseByDixonsMethod :: base -> Data.PrimeFactors.Factors base exponent
factoriseByDixonsMethod	= undefined

{- |
	* <https://en.wikipedia.org/wiki/Fermat%27s_factorization_method>.

	* <http://mathworld.wolfram.com/FermatsFactorizationMethod.html>.

	* <https://en.wikipedia.org/wiki/Congruence_of_squares>.

	*	@i = f1 * f2@							Assume a non-trivial factorisation, ie. one in which both factors exceed one.
	=>	@i = (larger + smaller) * (larger - smaller)@			Represent the co-factors as a sum and difference.
	=>	@i = larger^2 - smaller^2@					Which has an integral solution if @i@ is neither /even/ nor a /perfect square/.
	=>	@sqrt (larger^2 - i) = smaller@					Search for /larger/, which results in an integral value for /smaller/.

	* Given that the smaller factor /f2/, can't be less than 3 (/i/ isn't /even/), then the larger /f1/, can't be greater than @(i `div` 3)@.
	So:	@(f2 >= 3) && (f1 <= i `div` 3)@				Two equations which can be used to solve for /larger/.
	=>	@(larger - smaller >= 3) && (larger + smaller <= i `div` 3)@	Add these to eliminate /smaller/.
	=>	@larger <= (i + 9) `div` 6@					The upper bound of the search-space.

	* This algorithm works best when there's a factor close to the /square-root/.
-}
factoriseByFermatsMethod :: (
	Control.DeepSeq.NFData	base,
	Control.DeepSeq.NFData	exponent,
	Integral		base,
	Num			exponent
 ) => base -> Data.PrimeFactors.Factors base exponent
factoriseByFermatsMethod i
	| i <= 3				= [Data.Exponential.rightIdentity i]
	| even i				= Data.Exponential.rightIdentity 2 : factoriseByFermatsMethod (i `div` 2) {-recurse-}
	| Data.Maybe.isJust maybeSquareNumber	= (<^ 2) `map` factoriseByFermatsMethod (Data.Maybe.fromJust maybeSquareNumber) {-recurse-}
	| null factors				= [Data.Exponential.rightIdentity i]	-- Prime.
	| otherwise				= uncurry (++) . Control.Parallel.Strategies.withStrategy (
		Control.Parallel.Strategies.parTuple2 Control.Parallel.Strategies.rdeepseq Control.Parallel.Strategies.rdeepseq	-- CAVEAT: unproductive on the size of integers tested so far.
	) . ToolShed.Data.Pair.mirror factoriseByFermatsMethod $ head factors
	where
--		maybeSquareNumber :: Integral i => Maybe i
		maybeSquareNumber	= Math.PerfectPower.maybeSquareNumber i

--		factors :: Integral i => [i]
		factors	= map (
			(
				uncurry (+) &&& uncurry (-)	-- Construct the co-factors as the sum and difference of /larger/ and /smaller/.
			) . Control.Arrow.second Data.Maybe.fromJust
		 ) . filter (
			Data.Maybe.isJust . snd	-- Search for a perfect square.
		 ) . map (
			Control.Arrow.second $ Math.PerfectPower.maybeSquareNumber {-hotspot-} . subtract i	-- Associate the corresponding value of /smaller/.
		 ) . takeWhile (
			(<= (i + 9) `div` 6) . fst	-- Terminate the search at the maximum value of /larger/.
		 ) . Math.Power.squaresFrom {-hotspot-} . ceiling $ sqrt (fromIntegral i :: Double)	-- Start the search at the minimum value of /larger/.

{- |
	* Decomposes the specified integer, into a product of /prime/-factors,
	using <http://mathworld.wolfram.com/DirectSearchFactorization.html>, AKA <https://en.wikipedia.org/wiki/Trial_division>.

	* This works best when the factors are small.
-}
factoriseByTrialDivision :: (Integral base, Num exponent) => base -> Data.PrimeFactors.Factors base exponent
factoriseByTrialDivision	= slave Data.Numbers.Primes.primes where
	slave primes i
		| null primeCandidates	= [Data.Exponential.rightIdentity i]
		| otherwise		= Data.Exponential.rightIdentity lowestPrimeFactor `Data.PrimeFactors.insert'` slave primeCandidates (i `quot` lowestPrimeFactor)
		where
			primeCandidates	= dropWhile (
				(/= 0) . (i `rem`)
			 ) $ takeWhile (
				<= Math.PrimeFactorisation.maxBoundPrimeFactor i
			 ) primes

			lowestPrimeFactor	= head primeCandidates

