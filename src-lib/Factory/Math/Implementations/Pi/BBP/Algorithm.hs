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

 [@DESCRIPTION@]	Defines the set of /Bailey-Borwein-Plouffe/-type formulae which have been implemented.
-}

module Factory.Math.Implementations.Pi.BBP.Algorithm(
-- * Types
-- ** Data-types
	Algorithm(..)
) where

import qualified	Factory.Math.Implementations.Pi.BBP.Base65536		as Math.Implementations.Pi.BBP.Base65536
import qualified	Factory.Math.Implementations.Pi.BBP.Bellard		as Math.Implementations.Pi.BBP.Bellard
import qualified	Factory.Math.Implementations.Pi.BBP.Implementation	as Math.Implementations.Pi.BBP.Implementation
import qualified	Factory.Math.Pi						as Math.Pi
import qualified	ToolShed.Defaultable

-- | Defines those /BBP/-type series which have been implemented.
data Algorithm	=
	Base65536	-- ^ A /base/-@2^16@ version of the formula.
	| Bellard	-- ^ A /nega-base/ @2^10@ version of the formula.
	deriving (Eq, Read, Show)

instance ToolShed.Defaultable.Defaultable Algorithm	where
	defaultValue	= Base65536

instance Math.Pi.Algorithmic Algorithm	where
	openR Base65536	= Math.Implementations.Pi.BBP.Implementation.openR Math.Implementations.Pi.BBP.Base65536.series
	openR Bellard	= Math.Implementations.Pi.BBP.Implementation.openR Math.Implementations.Pi.BBP.Bellard.series

