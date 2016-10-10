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

 [@DESCRIPTION@]	Times the methods exported from module "Math.Pi".
-}

module Factory.Test.Performance.Pi(
-- * Types
-- ** Type-synonyms
	Category,
-- * Functions
	piPerformance,
	piPerformanceGraph
) where

import qualified	Factory.Math.Factorial					as Math.Factorial
import qualified	Factory.Math.Implementations.Pi.AGM.Algorithm		as Math.Implementations.Pi.AGM.Algorithm
import qualified	Factory.Math.Implementations.Pi.BBP.Algorithm		as Math.Implementations.Pi.BBP.Algorithm
import qualified	Factory.Math.Implementations.Pi.Borwein.Algorithm	as Math.Implementations.Pi.Borwein.Algorithm
import qualified	Factory.Math.Implementations.Pi.Ramanujan.Algorithm	as Math.Implementations.Pi.Ramanujan.Algorithm
import qualified	Factory.Math.Implementations.Pi.Spigot.Algorithm	as Math.Implementations.Pi.Spigot.Algorithm
import qualified	Factory.Math.Pi						as Math.Pi
import qualified	Factory.Math.Precision					as Math.Precision
import qualified	Factory.Math.SquareRoot					as Math.SquareRoot
import qualified	ToolShed.System.TimePure

-- | The type of a /Pi/-algorithm, including where required, the algorithm for /square-root/s and /factorial/s.
type Category squareRootAlgorithm factorialAlgorithm = Math.Pi.Category (
	Math.Implementations.Pi.AGM.Algorithm.Algorithm squareRootAlgorithm
 ) Math.Implementations.Pi.BBP.Algorithm.Algorithm (
	Math.Implementations.Pi.Borwein.Algorithm.Algorithm squareRootAlgorithm factorialAlgorithm
 ) (
	Math.Implementations.Pi.Ramanujan.Algorithm.Algorithm squareRootAlgorithm factorialAlgorithm
 ) Math.Implementations.Pi.Spigot.Algorithm.Algorithm

-- | Measures the CPU-time required to find Pi to the required precision.
piPerformance :: (
	Math.SquareRoot.Algorithmic	squareRootAlgorithm,
	Math.Factorial.Algorithmic	factorialAlgorithm
 ) => Category squareRootAlgorithm factorialAlgorithm -> Math.Precision.DecimalDigits -> IO (Double, String)
piPerformance category = ToolShed.System.TimePure.getCPUSeconds . Math.Pi.openS category

{- |
	* Measures the CPU-time required to determine /Pi/ to an exponentially increasing precision-requirement.

	* CAVEAT: nothing is returned, since the result is printed ... and it never terminates.
-}
piPerformanceGraph :: (
	Math.SquareRoot.Algorithmic	squareRootAlgorithm,
	Math.Factorial.Algorithmic	factorialAlgorithm
 ) => RealFrac i
	=> Category squareRootAlgorithm factorialAlgorithm	-- ^ The algorithm.
	-> i							-- ^ The factor by which the precision is increased on each iteration.
	-> Math.Precision.DecimalDigits				-- ^ The maximum precision required.
	-> Bool							-- ^ Whether to return the digits of /Pi/.
	-> IO ()
piPerformanceGraph category factor maxDecimalDigits verbose	= mapM_ (
	\decimalDigits	-> piPerformance category decimalDigits >>= putStrLn . shows decimalDigits . showChar '\t' . (
		if verbose
			then (`shows` "")
			else (`shows` "") . fst
	)
 ) . takeWhile (<= maxDecimalDigits) . map round $ iterate (* factor) 1
