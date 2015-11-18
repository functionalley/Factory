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

 [@DESCRIPTION@]	Defines a /Bailey-Borwein-Plouffe/ formula; <http://mathworld.wolfram.com/PiFormulas.html>
-}

module Factory.Math.Implementations.Pi.BBP.Series(
-- * Types
-- ** Data-types
	Series(..)
) where

-- | Defines a series corresponding to a specific /BBP/-formula.
data Series	= MkSeries {
	numerators		:: [Integer],		-- ^ The constant numerators from which each term in the series is composed.
	getDenominators		:: Int -> [Integer],	-- ^ Generates the term-dependent denominators from which each term in the series is composed.
	seriesScalingFactor	:: Rational,		-- ^ The ratio by which the sum to infinity of the series, must be scaled to result in /Pi/.
	base			:: Integer		-- ^ The geometric ratio, by which successive terms are scaled.
}

