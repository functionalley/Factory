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

 [@DESCRIPTION@]	Defines the /Borwein/ series for /Pi/; <https://en.wikipedia.org/wiki/Borwein%27s_algorithm#Jonathan_Borwein_and_Peter_Borwein.27s_Version_.281993.29>
-}

module Factory.Math.Implementations.Pi.Borwein.Borwein1993(
-- * Constants
	series
) where

-- import		Control.Arrow((***))
import			Data.Ratio((%))
-- import		Factory.Data.PrimeFactors((>*<), (>/<), (>^))
-- import qualified	Factory.Data.PrimeFactors			as Data.PrimeFactors
import qualified	Factory.Math.Factorial				as Math.Factorial
import qualified	Factory.Math.Implementations.Factorial		as Math.Implementations.Factorial
import qualified	Factory.Math.Implementations.Pi.Borwein.Series	as Math.Implementations.Pi.Borwein.Series
import qualified	Factory.Math.Power				as Math.Power
import qualified	Factory.Math.Precision				as Math.Precision
import qualified	Factory.Math.SquareRoot				as Math.SquareRoot

-- | Defines the parameters of the /Borwein/ series.
series :: (Math.SquareRoot.Algorithmic squareRootAlgorithm, Math.Factorial.Algorithmic factorialAlgorithm) => Math.Implementations.Pi.Borwein.Series.Series squareRootAlgorithm factorialAlgorithm
series = Math.Implementations.Pi.Borwein.Series.MkSeries {
	Math.Implementations.Pi.Borwein.Series.terms			= \squareRootAlgorithm factorialAlgorithm decimalDigits -> let
		simplify, squareRoot :: Rational -> Rational
		simplify	= Math.Precision.simplify $ pred decimalDigits {-ignore single integral digit-}	-- This makes a gigantic difference to performance.
		squareRoot	= simplify . Math.SquareRoot.squareRoot squareRootAlgorithm decimalDigits

		sqrt5, a, b, c3 :: Rational
		sqrt5	= squareRoot 5

		a	= 63365028312971999585426220 + sqrt5 * (28337702140800842046825600 + 384 * squareRoot (10891728551171178200467436212395209160385656017 + 4870929086578810225077338534541688721351255040 * sqrt5))
		b	= 7849910453496627210289749000 + 3510586678260932028965606400 * sqrt5 + 2515968 * squareRoot (3110 * (6260208323789001636993322654444020882161 + 2799650273060444296577206890718825190235 * sqrt5))
		c3	= simplify . Math.Power.cube $ negate 214772995063512240 - sqrt5 * (96049403338648032 + 1296 * squareRoot (10985234579463550323713318473 + 4912746253692362754607395912 * sqrt5))
	in (
		squareRoot $ negate c3,	-- The factor into which the series must be divided, to yield Pi.
		zipWith (
{-
			\n power -> let
				product'	= Data.PrimeFactors.product' (recip 2) 10
			in uncurry (/) . (
				(* (a + b * fromIntegral n)) . fromIntegral . product' *** (* power) . fromIntegral . product'
			) $ Math.Implementations.Factorial.primeFactors (6 * n) >/< (
				Math.Implementations.Factorial.primeFactors (3 * n) >*< Math.Implementations.Factorial.primeFactors n >^ 3
			)
-}
			\n power -> (
				Math.Implementations.Factorial.risingFactorial (succ $ 3 * n) (3 * n) % Math.Power.cube (Math.Factorial.factorial factorialAlgorithm n)
			) * (
				(a + b * fromIntegral n) / power
			)
		) [0 :: Integer ..] $ iterate (* c3) 1
	),
	Math.Implementations.Pi.Borwein.Series.convergenceRate		= 10 ** negate 50	-- Empirical.
}
