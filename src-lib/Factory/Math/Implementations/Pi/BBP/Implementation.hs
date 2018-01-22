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

	* Implements a /Bailey-Borwein-Plouffe/ formula; <https://mathworld.wolfram.com/PiFormulas.html>

	* Surprisingly, because of the huge size of the 'Rational' quantities,
	it is a /single/ call to @Factory.Math.Summation.sum'@, rather than the calculation of the many terms in the series, which is the performance-bottleneck.
-}

module Factory.Math.Implementations.Pi.BBP.Implementation(
-- * Functions
	openR
) where

import			Data.Ratio((%))
import qualified	Factory.Math.Implementations.Pi.BBP.Series	as Math.Implementations.Pi.BBP.Series
import qualified	Factory.Math.Precision				as Math.Precision
import qualified	Factory.Math.Summation				as Math.Summation

-- | Returns /Pi/, accurate to the specified number of decimal digits.
openR
	:: Math.Implementations.Pi.BBP.Series.Series	-- ^ This /Pi/-algorithm is parameterised by the type of other algorithms to use.
	-> Math.Precision.DecimalDigits			-- ^ The number of decimal digits required.
	-> Rational
openR Math.Implementations.Pi.BBP.Series.MkSeries {
	Math.Implementations.Pi.BBP.Series.numerators		= numerators,
	Math.Implementations.Pi.BBP.Series.getDenominators	= getDenominators,
	Math.Implementations.Pi.BBP.Series.seriesScalingFactor	= seriesScalingFactor,
	Math.Implementations.Pi.BBP.Series.base			= base
} decimalDigits		= (seriesScalingFactor *) . Math.Summation.sum' 8 . take (
	Math.Precision.getTermsRequired (
		recip . fromIntegral $ abs {-potentially negative-} base	-- The convergence-rate.
	) decimalDigits
 ) . zipWith (*) (
	iterate (/ fromIntegral base) 1	-- Generate the scaling-ratio, between successive terms.
 ) $ map (
	sum . zipWith (%) numerators . getDenominators
 ) [0 ..]

