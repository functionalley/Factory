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

 [@DESCRIPTION@]	Defines a specific base-@2^16@ /BBP/-formula; <http://mathworld.wolfram.com/PiFormulas.html>

-}

module Factory.Math.Implementations.Pi.BBP.Base65536(
-- * Constants
	series
) where

import qualified	Factory.Math.Implementations.Pi.BBP.Series	as Math.Implementations.Pi.BBP.Series

-- | Defines the parameters of this specific series.
series :: Math.Implementations.Pi.BBP.Series.Series
series	= Math.Implementations.Pi.BBP.Series.MkSeries {
	Math.Implementations.Pi.BBP.Series.numerators		= zipWith ($) (cycle [id, id, id, negate]) $ map (2 ^) [15 :: Integer, 14, 14, 12, 11, 10, 10, 8, 7, 6, 6, 4, 3, 2, 2, 0],
	Math.Implementations.Pi.BBP.Series.getDenominators	= \i -> map (32 * fromIntegral i +) [2, 3, 4, 7, 10, 11, 12, 15, 18, 19, 20, 23, 26, 27, 28, 31],
	Math.Implementations.Pi.BBP.Series.seriesScalingFactor	= recip $ 2 ^ (13 :: Int),
	Math.Implementations.Pi.BBP.Series.base			= 2 ^ (16 :: Int)
}
