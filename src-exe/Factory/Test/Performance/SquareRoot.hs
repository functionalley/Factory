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

 [@DESCRIPTION@]	Measures the CPU-time required by the methods exported from module "Math.SquareRoot".
-}

module Factory.Test.Performance.SquareRoot(
-- * Functions
	squareRootPerformance,
	squareRootPerformanceGraph
) where

import qualified	Control.Arrow
import qualified	Factory.Math.Precision	as Math.Precision
import qualified	Factory.Math.SquareRoot	as Math.SquareRoot
import qualified	ToolShed.System.TimePure

-- | Measures the CPU-time required by 'Math.SquareRoot.squareRootFrom', which is returned together with the approximate rational result.
squareRootPerformance :: (
	Math.SquareRoot.Algorithmic	algorithm,
	Real				operand,
	Show				operand
 ) => algorithm -> operand -> Math.Precision.DecimalDigits -> IO (Double, Math.SquareRoot.Result)
squareRootPerformance algorithm operand requiredDecimalDigits = ToolShed.System.TimePure.getCPUSeconds $ Math.SquareRoot.squareRoot algorithm requiredDecimalDigits operand

{- |
	* Measures the CPU-time required by 'Math.SquareRoot.squareRootFrom', and the resulting accuracy,
	using the specified algorithm, to an exponentially increasing precision-requirement.

	* CAVEAT: nothing is returned, since the result is printed ... and it never terminates.
-}
squareRootPerformanceGraph :: (
	Math.SquareRoot.Algorithmic	algorithm,
	Math.SquareRoot.Iterator	algorithm,
	Real				operand,
	Show				algorithm,
	Show				operand
 ) => algorithm -> operand -> IO ()
squareRootPerformanceGraph algorithm operand	= mapM_ (
	\requiredDecimalDigits	-> putStrLn . (
		\(cpuSeconds, actualDecimalDigits)	-> shows algorithm . showChar '\t' . shows requiredDecimalDigits . showChar '\t' . shows actualDecimalDigits . showChar '\t' $ shows cpuSeconds ""
	) . Control.Arrow.second (Math.SquareRoot.getAccuracy operand) =<< squareRootPerformance algorithm operand requiredDecimalDigits
 ) $ iterate (* max 2 (Math.SquareRoot.convergenceOrder algorithm)) 16
