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

 [@DESCRIPTION@]	Defines the /Gosper/ series; <http://www.pi314.net/eng/goutte.php>
-}

module Factory.Math.Implementations.Pi.Spigot.Gosper(
-- * Constants
	series
) where

import qualified	Factory.Math.Implementations.Pi.Spigot.Series	as Math.Implementations.Pi.Spigot.Series
import qualified	Factory.Math.Precision				as Math.Precision

-- | Defines a series which converges to /Pi/.
series :: Integral i => Math.Implementations.Pi.Spigot.Series.Series i
series	= Math.Implementations.Pi.Spigot.Series.MkSeries {
	Math.Implementations.Pi.Spigot.Series.baseNumerators	= map (\i -> i * pred (2 * i)) [1 ..],
	Math.Implementations.Pi.Spigot.Series.baseDenominators	= map ((* 3) . (\i -> succ i * (i + 2))) [3, 6 ..],
	Math.Implementations.Pi.Spigot.Series.coefficients	= [3, 8 ..],	-- 5n - 2
	Math.Implementations.Pi.Spigot.Series.nTerms		= Math.Precision.getTermsRequired $ 1 / 13 {-empirical convergence-rate-}
}

