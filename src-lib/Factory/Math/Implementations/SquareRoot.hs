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
	along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}
{- |
 [@AUTHOR@]	Dr. Alistair Ward

 [@DESCRIPTION@]	Implements 'Math.SquareRoot.Algorithmic' by a variety of methods.

 [@CAVEAT@]

	Caller may benefit from application of 'Math.Precision.simplify' before operating on the result;
	which though of the required accuracy, may not be the most concise rational number satisfying that criterion.
-}
module Factory.Math.Implementations.SquareRoot(
-- * Types
-- ** Type-synonyms
--	ProblemSpecification,
	Terms,
-- ** Data-types
	Algorithm(..)
-- * Functions
--	squareRootByContinuedFraction,
--	squareRootByIteration,
--	squareRootByTaylorSeries,
--	taylorSeriesCoefficients
) where

import			Control.Arrow((***))
import qualified	Data.Default
import			Factory.Data.PrimeFactors((>/<), (>^))
import qualified	Factory.Data.PrimeFactors		as Data.PrimeFactors
import qualified	Factory.Math.Implementations.Factorial	as Math.Implementations.Factorial
import qualified	Factory.Math.Power			as Math.Power
import qualified	Factory.Math.Precision			as Math.Precision
import qualified	Factory.Math.SquareRoot			as Math.SquareRoot
import qualified	Factory.Math.Summation			as Math.Summation

-- | The number of terms in a series.
type Terms	= Int

-- | The algorithms by which the /square-root/ has been implemented.
data Algorithm
	= BakhshaliApproximation	-- ^ <https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Bakhshali_approximation>
	| ContinuedFraction		-- ^ <https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion>.
	| HalleysMethod			-- ^ <https://en.wikipedia.org/wiki/Halley%27s_method>.
	| NewtonRaphsonIteration	-- ^ <https://en.wikipedia.org/wiki/Newton%27s_method>.
	| TaylorSeries Terms		-- ^ <https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Taylor_series>.
	deriving (Eq, Read, Show)

instance Data.Default.Default Algorithm	where
	def	= NewtonRaphsonIteration

-- | Returns an improved estimate for the /square-root/ of the specified value, to the required precision, using the supplied initial estimate..
type ProblemSpecification operand
	= Math.SquareRoot.Estimate
	-> Math.Precision.DecimalDigits	-- ^ The required precision.
	-> operand			-- ^ The value for which to find the /square-root/.
	-> Math.SquareRoot.Result

instance Math.SquareRoot.Algorithmic Algorithm	where
	squareRootFrom _ _ _ 0	= 0
	squareRootFrom _ _ _ 1	= 1
	squareRootFrom algorithm estimate@(x, decimalDigits) requiredDecimalDigits y
		| decimalDigits >= requiredDecimalDigits	= x
		| requiredDecimalDigits <= 0			= error $ "Factory.Math.Implementations.SquareRoot.squareRootFrom:\tinvalid number of required decimal digits; " ++ show requiredDecimalDigits
		| y < 0						= error $ "Factory.Math.Implementations.SquareRoot.squareRootFrom:\tthere's no real square-root of " ++ show y
		| otherwise					= (
			case algorithm of
				ContinuedFraction	-> squareRootByContinuedFraction
				_			-> squareRootByIteration algorithm
		) estimate requiredDecimalDigits y

instance Math.SquareRoot.Iterator Algorithm where
	step BakhshaliApproximation y x
		| dy == 0	= x		-- The estimate was precise.
		| otherwise	= x' - dx'	-- Correct the estimate.
		where
			dy, dydx, dx, x', dydx', dx' :: Math.SquareRoot.Result
			dy	= Math.SquareRoot.getDiscrepancy y x
			dydx	= 2 * x
			dx	= dy / dydx
			x'	= x + dx	-- Identical to Newton-Raphson iteration.
			dydx'	= 2 * x'
			dx'	= Math.Power.square dx / dydx'

{-
	* /Halley's/ method; <https://mathworld.wolfram.com/HalleysMethod.html>

>		X(n+1) = Xn - f(Xn) / [f'(Xn) - f''(Xn) * f(Xn) / 2 * f'(Xn)]
>			=> Xn - (Xn^2 - Y) / [2Xn - 2 * (Xn^2 - Y) / 2 * 2Xn] where Y = X^2, f(X) = X^2 - Y, f'(X) = 2X, f''(X) = 2
>			=> Xn - 1 / [2Xn / (Xn^2 - Y) - 1 / 2Xn]
-}
	step HalleysMethod y x
		| dy == 0	= x		-- The estimate was precise.
		| otherwise	= x - dx	-- Correct the estimate.
		where
			dy, dydx, dx :: Math.SquareRoot.Result
			dy	= negate $ Math.SquareRoot.getDiscrepancy y x	-- Use the estimate to determine the error in 'y'.
			dydx	= 2 * x						-- The gradient, at the estimated value 'x'.
			dx	= recip $ dydx / dy - recip dydx

--	step NewtonRaphsonIteration y x	= (x + toRational y / x) / 2		-- This is identical to the /Babylonian Method/.
--	step NewtonRaphsonIteration y x	= x / 2 + toRational y / (2 * x)	-- Faster.
	step NewtonRaphsonIteration y x	= x / 2 + (toRational y / 2) / x	-- Faster still.

	step (TaylorSeries terms) y x	= squareRootByTaylorSeries terms y x

	step algorithm _ _		= error $ "Factory.Math.Implementations.SquareRoot.step:\tinappropriate algorithm; " ++ show algorithm

	convergenceOrder BakhshaliApproximation	= Math.Precision.quarticConvergence
	convergenceOrder ContinuedFraction	= Math.Precision.linearConvergence
	convergenceOrder HalleysMethod		= Math.Precision.cubicConvergence
	convergenceOrder NewtonRaphsonIteration	= Math.Precision.quadraticConvergence
	convergenceOrder (TaylorSeries terms)	= terms	-- The order of convergence, per iteration, equals the number of terms in the series on each iteration.

{- |
	* Uses /continued-fractions/, to iterate towards the principal /square-root/ of the specified positive integer;
	<https://en.wikipedia.org/wiki/Solving_quadratic_equations_with_continued_fractions>,
	<https://en.wikipedia.org/wiki/Generalized_continued_fraction#Roots_of_positive_numbers>,
	<https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion>.
	<https://www.myreckonings.com/Dead_Reckoning/Online/Materials/General%20Method%20for%20Extracting%20Roots.pdf>

	* The convergence <https://en.wikipedia.org/wiki/Rate_of_convergence> of the /continued-fraction/ is merely /1st order/ (linear).
-}
squareRootByContinuedFraction :: Real operand => ProblemSpecification operand
squareRootByContinuedFraction (initialEstimate, initialDecimalDigits) requiredDecimalDigits y	= initialEstimate + (convergents initialEstimate !! Math.Precision.getTermsRequired (10 ^^ negate initialDecimalDigits) requiredDecimalDigits)	where
	convergents :: Math.SquareRoot.Result -> [Math.SquareRoot.Result]
	convergents x	= iterate ((Math.SquareRoot.getDiscrepancy y x /) . ((2 * x) +)) 0

{- |
	* The constant coefficients of the /Taylor-series/ for a /square-root/; <https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Taylor_series>.

	* @ ((-1)^n * factorial(2*n)) / ((1 - 2*n) * 4^n * factorial(n^2)) @.
-}
taylorSeriesCoefficients :: Fractional f => [f]
taylorSeriesCoefficients	= zipWith (
	\powers n	-> let
		doubleN		= 2 * n
		product'	= Data.PrimeFactors.product' (recip 2) {-arbitrary-} 10 {-arbitrary-}
	in uncurry (/) . (
		fromIntegral . product' *** fromIntegral . (* ((1 - doubleN) * powers)) . product'
	) $ Math.Implementations.Factorial.primeFactors doubleN >/< Math.Implementations.Factorial.primeFactors n >^ 2
 ) (
	iterate (* negate 4) 1	-- (-4)^n
 ) [0 :: Integer ..]		-- n

{- |
	* Returns the /Taylor-series/ for the /square-root/ of the specified value, to any requested number of terms.

	* <https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Taylor_series>.

	* The convergence of the series is merely /linear/,
	in that each term increases the precision, by a constant number of decimal places, equal to the those in the original estimate.

	* By feeding-back the improved estimate, to form a new series, the order of convergence, on each successive iteration,
	becomes proportional to the number of terms;

>		Terms		Convergence
>		=====		===========
>		2 terms		/quadratic/
>		3 terms		/cubic/
-}
squareRootByTaylorSeries :: Real operand
	=> Terms			-- ^ The number of terms of the infinite series, to evaluate.
	-> operand			-- ^ The value for which the /square-root/ is required.
	-> Math.SquareRoot.Result	-- ^ An initial estimate.
	-> Math.SquareRoot.Result
squareRootByTaylorSeries _ _ 0	= error "Factory.Math.Implementations.SquareRoot.squareRootByTaylorSeries:\talgorithm can't cope with estimated value of zero."
squareRootByTaylorSeries terms y x
	| terms < 2	= error $ "Factory.Math.Implementations.SquareRoot.squareRootByTaylorSeries:\tinvalid number of terms; " ++ show terms
	| otherwise	= Math.Summation.sumR' . take terms . zipWith (*) taylorSeriesCoefficients $ iterate (* relativeError) x
	where
		relativeError :: Math.SquareRoot.Result
		relativeError	= pred $ toRational y / Math.Power.square x	-- Pedantically, this is the error in y, which is twice the magnitude of the error in x.

-- | Iterates from the estimated value, towards the /square-root/, a sufficient number of times to achieve the required accuracy.
squareRootByIteration :: Real operand => Algorithm -> ProblemSpecification operand
squareRootByIteration algorithm (initialEstimate, initialDecimalDigits) requiredDecimalDigits y	= iterate (Math.SquareRoot.step algorithm y) initialEstimate !! Math.Precision.getIterationsRequired (Math.SquareRoot.convergenceOrder algorithm) initialDecimalDigits requiredDecimalDigits

