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

 [@DESCRIPTION@]	Defines the set of /Borwein/-type algorithms (currently only one) which have been implemented; <http://www.pi314.net/eng/borwein.php>.
-}

module Factory.Math.Implementations.Pi.Borwein.Algorithm(
-- * Types
-- ** Data-types
	Algorithm(..)
) where

import qualified	Data.Default
import qualified	Factory.Math.Factorial					as Math.Factorial
import qualified	Factory.Math.Implementations.Pi.Borwein.Borwein1993	as Math.Implementations.Pi.Borwein.Borwein1993
import qualified	Factory.Math.Implementations.Pi.Borwein.Implementation	as Math.Implementations.Pi.Borwein.Implementation
import qualified	Factory.Math.Pi						as Math.Pi
import qualified	Factory.Math.SquareRoot					as Math.SquareRoot

{- |
	* Define those /Borwein/-series which have been implemented.

	* Though currently there's only one, provision has been made for the addition of more.
-}
data Algorithm squareRootAlgorithm factorialAlgorithm	=
	Borwein1993 squareRootAlgorithm factorialAlgorithm	-- ^ <https://en.wikipedia.org/wiki/Borwein%27s_algorithm>.
	deriving (Eq, Read, Show)

instance (
	Data.Default.Default	squareRootAlgorithm,
	Data.Default.Default	factorialAlgorithm
 ) => Data.Default.Default (Algorithm squareRootAlgorithm factorialAlgorithm)	where
	def	= Borwein1993 Data.Default.def Data.Default.def

instance (
	Math.SquareRoot.Algorithmic	squareRootAlgorithm,
	Math.Factorial.Algorithmic	factorialAlgorithm
 ) => Math.Pi.Algorithmic (Algorithm squareRootAlgorithm factorialAlgorithm)	where
	openR (Borwein1993 squareRootAlgorithm factorialAlgorithm)	= Math.Implementations.Pi.Borwein.Implementation.openR Math.Implementations.Pi.Borwein.Borwein1993.series squareRootAlgorithm factorialAlgorithm

