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
	along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
{- |
 [@AUTHOR@]	Dr. Alistair Ward

 [@DESCRIPTION@]	Defines the set of /Spigot/-algorithms which have been implemented.
-}

module Factory.Math.Implementations.Pi.Spigot.Algorithm(
-- * Types
-- ** Data-types
	Algorithm(..)
) where

import qualified	Data.Default
import			Data.Ratio((%))
import qualified	Factory.Math.Implementations.Pi.Spigot.Gosper		as Math.Implementations.Pi.Spigot.Gosper
import qualified	Factory.Math.Implementations.Pi.Spigot.RabinowitzWagon	as Math.Implementations.Pi.Spigot.RabinowitzWagon
import qualified	Factory.Math.Implementations.Pi.Spigot.Spigot		as Math.Implementations.Pi.Spigot.Spigot
import qualified	Factory.Math.Pi						as Math.Pi

-- | Define those /Spigot/-algorithms which have been implemented.
data Algorithm
	= Gosper		-- ^ A /continued fraction/ discovered by /Gosper/.
	| RabinowitzWagon	-- ^ A /continued fraction/ discovered by /Rabinowitz/ and /Wagon/.
	deriving (Eq, Read, Show)

instance Data.Default.Default Algorithm	where
	def	= Gosper

instance Math.Pi.Algorithmic Algorithm	where
	openI Gosper			= Math.Implementations.Pi.Spigot.Spigot.openI Math.Implementations.Pi.Spigot.Gosper.series
	openI RabinowitzWagon		= Math.Implementations.Pi.Spigot.Spigot.openI Math.Implementations.Pi.Spigot.RabinowitzWagon.series

	openR algorithm decimalDigits	= Math.Pi.openI algorithm decimalDigits % (10 ^ pred decimalDigits)

