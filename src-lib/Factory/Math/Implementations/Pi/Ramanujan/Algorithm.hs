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

 [@DESCRIPTION@]	Defines the set of /Ramanujan/-type algorithms which have been implemented; <https://en.wikipedia.org/wiki/Pi>.
-}

module Factory.Math.Implementations.Pi.Ramanujan.Algorithm(
-- * Types
-- ** Data-types
	Algorithm(..)
) where

import qualified	Data.Default
import qualified	Factory.Math.Factorial						as Math.Factorial
import qualified	Factory.Math.Implementations.Pi.Ramanujan.Chudnovsky		as Math.Implementations.Pi.Ramanujan.Chudnovsky
import qualified	Factory.Math.Implementations.Pi.Ramanujan.Classic		as Math.Implementations.Pi.Ramanujan.Classic
import qualified	Factory.Math.Implementations.Pi.Ramanujan.Implementation	as Math.Implementations.Pi.Ramanujan.Implementation
import qualified	Factory.Math.Pi							as Math.Pi
import qualified	Factory.Math.SquareRoot						as Math.SquareRoot

-- | Define those /Ramanujan/-series which have been implemented.
data Algorithm squareRootAlgorithm factorialAlgorithm
	= Classic squareRootAlgorithm factorialAlgorithm	-- ^ The original version.
	| Chudnovsky squareRootAlgorithm factorialAlgorithm	-- ^ A variant found by the /Chudnovsky brothers/.
	deriving (Eq, Read, Show)

instance (
	Data.Default.Default	squareRootAlgorithm,
	Data.Default.Default	factorialAlgorithm
 ) => Data.Default.Default (Algorithm squareRootAlgorithm factorialAlgorithm)	where
	def	= Chudnovsky Data.Default.def Data.Default.def

instance (
	Math.SquareRoot.Algorithmic	squareRootAlgorithm,
	Math.Factorial.Algorithmic	factorialAlgorithm
 ) => Math.Pi.Algorithmic (Algorithm squareRootAlgorithm factorialAlgorithm)	where
	openR (Classic squareRootAlgorithm factorialAlgorithm)		= Math.Implementations.Pi.Ramanujan.Implementation.openR Math.Implementations.Pi.Ramanujan.Classic.series squareRootAlgorithm factorialAlgorithm
	openR (Chudnovsky squareRootAlgorithm factorialAlgorithm)	= Math.Implementations.Pi.Ramanujan.Implementation.openR Math.Implementations.Pi.Ramanujan.Chudnovsky.series squareRootAlgorithm factorialAlgorithm

