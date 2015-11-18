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

 [@DESCRIPTION@]	Times functions exported from module "Math.Primality".
-}

module Factory.Test.Performance.Primality(
-- * Functions
	carmichaelNumbersPerformance,
	isPrimePerformance,
	isPrimePerformanceGraph
) where

import qualified	Control.DeepSeq
import qualified	Factory.Math.Fibonacci	as Math.Fibonacci
import qualified	Factory.Math.Primality	as Math.Primality
import qualified	ToolShed.System.TimePure

-- | Measures the CPU-time required to find the specified number of /Carmichael/-numbers, which is returned together with the requested list.
carmichaelNumbersPerformance :: Math.Primality.Algorithmic primalityAlgorithm => primalityAlgorithm -> Int -> IO (Double, [Integer])
carmichaelNumbersPerformance primalityAlgorithm i
	| i < 0		= error . showString "Factory.Test.Performance.Primality.carmichaelNumbersPerformance:\tnegative number; " $ shows i "."
	| otherwise	= ToolShed.System.TimePure.getCPUSeconds . take i $ Math.Primality.carmichaelNumbers primalityAlgorithm

-- | Measures the CPU-time required to determine whether the specified integer is prime, which is returned together with the Boolean result.
isPrimePerformance :: (Control.DeepSeq.NFData i, Integral i, Show i) => Math.Primality.Algorithmic primalityAlgorithm => primalityAlgorithm -> i -> IO (Double, Bool)
isPrimePerformance primalityAlgorithm	= ToolShed.System.TimePure.getCPUSeconds . Math.Primality.isPrime primalityAlgorithm

{- |
	* Measures the CPU-time required to determine whether /prime-indexed Fibonacci-numbers/ are actually /prime/.

	* CAVEAT: nothing is returned, since the result is printed ... and it never terminates.
-}
isPrimePerformanceGraph :: Math.Primality.Algorithmic primalityAlgorithm => primalityAlgorithm -> IO ()
isPrimePerformanceGraph primalityAlgorithm	= mapM_ (
	\operand	-> isPrimePerformance primalityAlgorithm operand >>= putStrLn . shows operand . showChar '\t' . (`shows` "")
 ) (Math.Fibonacci.primeIndexedFibonacci :: [Integer])

