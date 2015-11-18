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

 [@DESCRIPTION@]	Defines the /Ramanujan/ series for /Pi/; <http://planetmath.org/encyclopedia/RamanujansFormulaForPi.html>.
-}

module Factory.Math.Implementations.Pi.Ramanujan.Classic(
-- * Constants
	series
) where

-- import		Control.Arrow((***))
import			Data.Ratio((%))
-- import		Factory.Data.PrimeFactors((>/<), (>^))
-- import qualified	Factory.Data.PrimeFactors				as Data.PrimeFactors
import qualified	Factory.Math.Factorial					as Math.Factorial
import qualified	Factory.Math.Implementations.Factorial			as Math.Implementations.Factorial
import qualified	Factory.Math.Implementations.Pi.Ramanujan.Series	as Math.Implementations.Pi.Ramanujan.Series
import qualified	Factory.Math.Power					as Math.Power
import qualified	Factory.Math.SquareRoot					as Math.SquareRoot

-- | Defines the parameters of the /Ramanujan/ series.
series :: (Math.SquareRoot.Algorithmic squareRootAlgorithm, Math.Factorial.Algorithmic factorialAlgorithm) => Math.Implementations.Pi.Ramanujan.Series.Series squareRootAlgorithm factorialAlgorithm
series = Math.Implementations.Pi.Ramanujan.Series.MkSeries {
	Math.Implementations.Pi.Ramanujan.Series.terms			= \factorialAlgorithm -> let
		toFourthPower	= (^ (4 :: Int))
	in zipWith (
{-
		\n power -> let
			product'	= Data.PrimeFactors.product' (recip 2) 10
		in uncurry (%) . (
			(* (1103 + 26390 * n)) . product' *** (* power) . product'
		) $ Math.Implementations.Factorial.primeFactors (4 * n) >/< Math.Implementations.Factorial.primeFactors n >^ 4
-}
		\n power -> (
			Math.Implementations.Factorial.risingFactorial (succ n) (3 * n) % Math.Power.cube (Math.Factorial.factorial factorialAlgorithm n)
		) * (
			(1103 + 26390 * n) % power
		) -- CAVEAT: the order in which these terms are evaluated radically affects performance.
	) [0 ..] $ iterate (* toFourthPower 396) 1,
	Math.Implementations.Pi.Ramanujan.Series.getSeriesScalingFactor	= \squareRootAlgorithm decimalDigits -> 9801 / Math.SquareRoot.squareRoot squareRootAlgorithm decimalDigits (8 :: Integer),
	Math.Implementations.Pi.Ramanujan.Series.convergenceRate	= 10 ** negate 7.9	-- Empirical.
}

