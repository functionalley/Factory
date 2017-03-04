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

	* <https://en.wikipedia.org/wiki/Integer_factorization>.

	* Exports a common interface to permit decomposition of positive integers,
	into the unique combination of /prime/-factors known to exist according to the /Fundamental Theorem of Arithmetic/; <https://en.wikipedia.org/wiki/Fundamental_theorem_of_arithmetic>.

	* Leveraging this abstract capability, it derives the /smoothness/, /power-smoothness/, /omega/-numbers and /square-free/ integers.

	* Filters the list of /regular-numbers/ from the list of /smoothness/.

	* CAVEAT: to avoid wasting time, it may be advantageous to check /Factory.Math.Primality.isPrime/ first.
-}

module Factory.Math.PrimeFactorisation(
-- * Type-classes
	Algorithmic(..),
-- * Functions
	maxBoundPrimeFactor,
	smoothness,
	powerSmoothness,
	regularNumbers,
	primePowerTotient,
	eulersTotient,
	omega,
	squareFree
) where

import qualified	Control.DeepSeq
import qualified	Factory.Data.Exponential	as Data.Exponential
import qualified	Factory.Data.PrimeFactors	as Data.PrimeFactors

-- | Defines the methods expected of a /factorisation/-algorithm.
class Algorithmic algorithm	where
	primeFactors	:: (Control.DeepSeq.NFData base, Integral base)
		=> algorithm
		-> base	-- ^ The operand
		-> Data.PrimeFactors.Factors base Int {-arbitrarily-}

{- |
	* The upper limit for a prime to be considered as a candidate factor of the specified number.

	* One might naively think that this limit is @(x `div` 2)@ for an even number,
	but though a prime-factor /greater/ than the /square-root/ of the number can exist,
	its smaller /cofactor/ decomposes to a prime which must be less than the /square-root/.

	* N.B.: rather then using @(primeFactor <= sqrt numerator)@ to filter the candidate prime-factors of a given numerator,
	one can alternatively use @(numerator >= primeFactor ^ 2)@ to filter what can potentially be factored by a given prime-factor.

	* CAVEAT: suffers from rounding-errors, though no consequence has been witnessed.
-}
maxBoundPrimeFactor :: Integral i => i -> i
maxBoundPrimeFactor	= floor . (sqrt :: Double -> Double) . fromIntegral

{- |
	* A constant, zero-indexed, conceptually infinite, list, of the /smooth/ness of all positive integers.

	* <https://en.wikipedia.org/wiki/Smooth_number>.

	* <http://mathworld.wolfram.com/SmoothNumber.html>.
-}
smoothness :: (Algorithmic algorithm, Control.DeepSeq.NFData base, Integral base) => algorithm -> [base]
smoothness algorithm	= 0 : map (Data.Exponential.getBase . last . primeFactors algorithm) [1 ..]

{- |
	* A constant, zero-indexed, conceptually infinite, list of the /power-smooth/ness of all positive integers.

	* <https://en.wikipedia.org/wiki/Smooth_number#Powersmooth_numbers>.
-}
powerSmoothness :: (Algorithmic algorithm, Control.DeepSeq.NFData base, Integral base) => algorithm -> [base]
powerSmoothness algorithm	= 0 : map (maximum . map Data.Exponential.evaluate . primeFactors algorithm) [1 ..]

{- |
	* Filters 'smoothness', to derive the constant list of /Hamming-numbers/.

	* <https://en.wikipedia.org/wiki/Regular_number>.
-}
regularNumbers :: (Algorithmic algorithm, Integral base) => algorithm -> [base]
regularNumbers algorithm	= map fst . filter ((<= (5 :: Integer)) . snd) . zip [1 ..] . tail $ smoothness algorithm

{- |
	* /Euler's Totient/ for a /power/ of a /prime/-number.

	* By /Olofsson/; @(phi(n^k) = n^(k - 1) * phi(n))@
	and since @(phi(prime) = prime - 1)@

	* CAVEAT: checks neither the primality nor the bounds of the specified value; therefore for internal use only.
-}
primePowerTotient :: (Integral base, Integral exponent) => Data.Exponential.Exponential base exponent -> base
primePowerTotient (base, exponent')	= pred base * base ^ pred exponent'

{- |
	* The number of /coprimes/ less than or equal to the specified positive integer.

	* <https://en.wikipedia.org/wiki/Euler%27s_totient_function>.

	* <http://mathworld.wolfram.com/TotientFunction.html>.

	* AKA /EulerPhi/.
-}
eulersTotient :: (
	Algorithmic		algorithm,
	Control.DeepSeq.NFData	i,
	Integral		i,
	Show			i
 ) => algorithm -> i -> i
eulersTotient _ 1	= 1
eulersTotient algorithm i
	| i <= 0	= error $ "Factory.Math.PrimeFactorisation.eulersTotient:\tundefined for; " ++ show i
	| otherwise	= product . map primePowerTotient $ primeFactors algorithm i

{- |
	* A constant, zero-indexed, conceptually infinite, list of the /small omega/ numbers (i.e. the number of /distinct/ prime factors); cf. /big omega/.

	* <http://oeis.org/wiki/Omega%28n%29,_number_of_distinct_primes_dividing_n>.

	* <http://mathworld.wolfram.com/DistinctPrimeFactors.html>

	* <http://planetmath.org/encyclopedia/NumberOfDistinctPrimeFactorsFunction.html>.
-}
omega :: (Algorithmic algorithm, Integral i) => algorithm -> [i]
omega algorithm	= map (fromIntegral . length . primeFactors algorithm) [0 :: Integer ..]

{- |
	* A constant, conceptually infinite, list of the /square-free/ numbers, i.e. those which aren't divisible by any /perfect square/.

	* <https://en.wikipedia.org/wiki/Square-free_integer>.
-}
squareFree :: (Algorithmic algorithm, Control.DeepSeq.NFData i, Integral i) => algorithm -> [i]
squareFree algorithm	= filter (all (== 1) . map Data.Exponential.getExponent . primeFactors algorithm) [1 ..]

