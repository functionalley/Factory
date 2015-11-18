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

 [@DESCRIPTION@]	Defines the parameters of a series used in a /Spigot/-table to generate /Pi/.
-}

module Factory.Math.Implementations.Pi.Spigot.Series(
-- * Types
-- ** Data-types
	Series(..),
-- * Functions
	bases
) where

import			Data.Ratio((%))
import qualified	Data.Ratio
import qualified	Factory.Math.Precision	as Math.Precision

{- |
	* Defines a series composed from a sum of terms, each one of which is the product of a coefficient and a base.

	* The coefficents and bases of the series are described in /Horner form/; @Pi = c1 + (b1 * (c2 + b2 * (c3 + b3 * (...))))@.
-}
data Series i	= MkSeries {
	coefficients		:: [i],
	baseNumerators		:: [i],
	baseDenominators	:: [i],
	nTerms			:: Math.Precision.DecimalDigits -> Int	-- ^ The width of the spigot-table, required to accurately generate the requested number of digits.
}

-- | Combines 'baseNumerators' and 'baseDenominators', and as a side-effect, expresses the ratio in lowest terms.
bases :: Integral i => Series i -> [Data.Ratio.Ratio i]
bases MkSeries {
	baseNumerators		= n,
	baseDenominators	= d
} = zipWith (%) n d

