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

 [@DESCRIPTION@]	Defines /Bellard/'s nega-base-@2^10@ /BBP/-formula; <https://en.wikipedia.org/wiki/Bellard%27s_formula>
-}

module Factory.Math.Implementations.Pi.BBP.Bellard(
-- * Constants
	series
) where

import			Control.Arrow((&&&))
import qualified	Factory.Math.Implementations.Pi.BBP.Series	as Math.Implementations.Pi.BBP.Series

-- | Defines the parameters of this specific series.
series :: Math.Implementations.Pi.BBP.Series.Series
series	= Math.Implementations.Pi.BBP.Series.MkSeries {
	Math.Implementations.Pi.BBP.Series.numerators		= zipWith ($) [negate, negate, id, negate, negate, negate, id] $ map (2 ^) [5 :: Integer, 0, 8, 6, 2, 2, 0],
	Math.Implementations.Pi.BBP.Series.getDenominators	= \i -> let
		f, t :: Integer
		(f, t)	= (4 *) &&& (10 *) $ fromIntegral i
	in [f + 1, f + 3, t + 1, t + 3, t + 5, t + 7, t + 9],
	Math.Implementations.Pi.BBP.Series.seriesScalingFactor	= recip $ 2 ^ (6 :: Int),
	Math.Implementations.Pi.BBP.Series.base			= negate $ 2 ^ (10 :: Int)
}
