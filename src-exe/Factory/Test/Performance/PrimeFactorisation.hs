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

 [@DESCRIPTION@]	Times the methods exported by module "Math.PrimeFactorisation".
-}

module Factory.Test.Performance.PrimeFactorisation(
-- * Functions
	primeFactorsPerformance,
	primeFactorsPerformanceGraph
) where

import qualified	Factory.Data.PrimeFactors	as Data.PrimeFactors
import qualified	Factory.Math.Fibonacci		as Math.Fibonacci
import qualified	Factory.Math.PrimeFactorisation	as Math.PrimeFactorisation
import qualified	ToolShed.System.TimePure

-- | Measures the CPU-time required to prime-factorise the specified integer, which is returned together with the resulting list of factors.
primeFactorsPerformance :: Math.PrimeFactorisation.Algorithmic algorithm => algorithm -> Integer -> IO (Double, Data.PrimeFactors.Factors Integer Int)
primeFactorsPerformance algorithm	= ToolShed.System.TimePure.getCPUSeconds . Math.PrimeFactorisation.primeFactors algorithm

{- |
	* Measure the CPU-time required by 'Math.PrimeFactorisation.primeFactors',
	arbitrarily against the /Fibonacci/-numbers (which seemed to fit the requirements).

	* CAVEAT: nothing is returned, since the result is printed ... and it never terminates.
-}
primeFactorsPerformanceGraph :: Math.PrimeFactorisation.Algorithmic algorithm => algorithm -> Int -> IO ()
primeFactorsPerformanceGraph algorithm tests
	| tests < 0	= error . showString "Factory.Test.Performance.PrimeFactorisation.primeFactorsPerformanceGraph:\tnegative number; " $ shows tests "."
	| otherwise	= mapM_ (
		\operand	-> primeFactorsPerformance algorithm operand >>= putStrLn . shows operand . showChar '\t' . (`shows` "")
	) . take tests . dropWhile (< 2) $ Math.Fibonacci.fibonacci

