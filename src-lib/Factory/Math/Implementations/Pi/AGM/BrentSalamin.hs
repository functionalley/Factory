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
	along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}
{- |
 [@AUTHOR@]	Dr. Alistair Ward

 [@DESCRIPTION@]

	* Implements the /Brent-Salamin/ (AKA /Gauss-Legendre/) algorithm;
		<https://en.wikipedia.org/wiki/Gauss%E2%80%93Legendre_algorithm>,
		<https://mathworld.wolfram.com/Brent-SalaminFormula.html>,
		<https://www.pi314.net/eng/salamin.php>.

	* The precision of the result approximately doubles for each iteration.

 [@CAVEAT@]	Assumptions on the convergence-rate result in rounding-errors, when only a small number of digits are requested.
-}

module Factory.Math.Implementations.Pi.AGM.BrentSalamin(
-- * Functions
	openR
) where

import			Control.Arrow((&&&))
import qualified	Factory.Math.ArithmeticGeometricMean	as Math.ArithmeticGeometricMean
import qualified	Factory.Math.Power			as Math.Power
import qualified	Factory.Math.Precision			as Math.Precision
import qualified	Factory.Math.SquareRoot			as Math.SquareRoot

{- |
	* Returns /Pi/, accurate to the specified number of decimal digits.

	* This algorithm is based on the /arithmetic-geometric/ mean of @1@ and @(1 / sqrt 2)@,
	but there are many confusingly similar formulations.
	The algorithm I've used here, where @a@ is the /arithmetic mean/ and @g@ is the /geometric mean/, is equivalent to other common formulations:

>		pi = (a[N-1] + g[N-1])^2 / (1 - sum [2^n * (a[n] - g[n])^2])			where n = [0 .. N-1]
>		=> 4*a[N]^2 / (1 - sum [2^n * (a[n]^2 - 2*a[n]*g[n] + g[n]^2)])
>		=> 4*a[N]^2 / (1 - sum [2^n * (a[n]^2 + 2*a[n]*g[n] + g[n]^2 - 4*a[n]*g[n])])
>		=> 4*a[N]^2 / (1 - sum [2^n * ((a[n] + g[n])^2 - 4*a[n]*g[n])])
>		=> 4*a[N]^2 / (1 - sum [2^(n-1) * 4 * (a[n-1]^2 - g[n-1]^2)])			where n = [1 .. N]
>		=> 4*a[N]^2 / (1 - sum [2^(n+1) * (a[n-1]^2 - g[n-1]^2)])

-}
openR :: Math.SquareRoot.Algorithmic squareRootAlgorithm => squareRootAlgorithm -> Math.Precision.DecimalDigits -> Rational
openR squareRootAlgorithm decimalDigits	= uncurry (/) . (
	Math.Power.square . uncurry (+) . last &&& negate . pred . sum . zipWith (*) (iterate (* 2) 1) . map (Math.Power.square . Math.ArithmeticGeometricMean.spread)
 ) . take (
	Math.Precision.getIterationsRequired Math.Precision.quadraticConvergence 1 decimalDigits
 ) $ Math.ArithmeticGeometricMean.convergeToAGM squareRootAlgorithm decimalDigits (1, Math.SquareRoot.squareRoot squareRootAlgorithm decimalDigits (recip 2 :: Rational))

