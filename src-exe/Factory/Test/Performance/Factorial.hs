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

 [@DESCRIPTION@]	Times the methods exported from module "Math.Factorial".
-}

module Factory.Test.Performance.Factorial(
-- * Functions
	factorialPerformance,
	factorialPerformanceControl,
	factorialPerformanceGraph,
	factorialPerformanceGraphControl
) where

import qualified	Control.DeepSeq
import qualified	Data.List
import qualified	Factory.Math.Factorial	as Math.Factorial
import qualified	ToolShed.System.TimePure

-- | Measures the CPU-time required by 'Math.Factorial.factorial'.
factorialPerformance :: (
	Control.DeepSeq.NFData		i,
	Integral			i,
	Math.Factorial.Algorithmic	algorithm,
	Show				i
 ) => algorithm -> i -> IO (Double, i)
factorialPerformance algorithm	= ToolShed.System.TimePure.getCPUSeconds . Math.Factorial.factorial algorithm

-- | Measures the CPU-time required by a naive implementation.
factorialPerformanceControl :: (Control.DeepSeq.NFData i, Integral i) => i -> IO (Double, i)
-- factorialPerformanceControl i	= ToolShed.System.TimePure.getCPUSeconds $ product [1 .. i]	-- CAVEAT: too lazy.
factorialPerformanceControl i	= ToolShed.System.TimePure.getCPUSeconds $ Data.List.foldl' (*) 1 [2 .. i]

{- |
	* Measure the CPU-time required by 'Math.Factorial.factorial', against an exponentially increasing operand.

	* CAVEAT: nothing is returned, since the result is printed ... and it never terminates.
-}
factorialPerformanceGraph :: Math.Factorial.Algorithmic algorithm => Bool -> algorithm -> IO ()
factorialPerformanceGraph verbose algorithm	= mapM_ (
	\operand	-> factorialPerformance algorithm operand >>= putStrLn . shows operand . showChar '\t' . (
		if verbose
			then (`shows` "")
			else (`shows` "") . fst
	)
 ) $ iterate (* 2) (1 :: Integer)

-- | Graphs the CPU-time required by a naive implementation, against an exponentially increasing operand.
factorialPerformanceGraphControl :: Bool -> IO ()
factorialPerformanceGraphControl verbose	= mapM_ (
	\operand	-> factorialPerformanceControl operand >>= putStrLn . shows operand . showChar '\t' . (
		if verbose
			then (`shows` "")
			else (`shows` "") . fst
	)
 ) $ iterate (* 2) (1 :: Integer)

