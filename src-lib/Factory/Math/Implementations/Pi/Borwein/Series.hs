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

 [@DESCRIPTION@]	Defines a <http://en.wikipedia.org/wiki/Srinivasa_Borwein>-type series for /Pi/.
-}

module Factory.Math.Implementations.Pi.Borwein.Series(
-- * Types
-- ** Data-types
	Series(..)
) where

import qualified	Factory.Math.Precision	as Math.Precision

-- | Defines a series corresponding to a specific /Borwein/-formula.
data Series squareRootAlgorithm factorialAlgorithm	= MkSeries {
	terms
		:: squareRootAlgorithm
		-> factorialAlgorithm
		-> Math.Precision.DecimalDigits
		-> (
			Rational,	-- The factor into which the sum to infinity of the sequence, must be divided to result in /Pi/
			[Rational]	-- The sequence of terms, the sum to infinity of which defines the series.
		),
	convergenceRate :: Math.Precision.ConvergenceRate	-- ^ The expected number of digits of /Pi/, per term in the series.
}

