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

 [@DESCRIPTION@]	Measures the CPU-time required by "Math.Primes.primes".
-}

module Factory.Test.Performance.Primes(
-- * Functions
	primesPerformance,
	mersenneNumbersPerformance
) where

import qualified	Control.DeepSeq
import qualified	Data.Array.IArray
import qualified	Factory.Math.Primes	as Math.Primes
import qualified	ToolShed.System.TimePure

-- | Measures the CPU-time required by 'Math.Primes.primes', to find the specified prime.
primesPerformance :: (
	Control.DeepSeq.NFData	i,
	Data.Array.IArray.Ix	i,
	Math.Primes.Algorithmic	algorithm,
	Integral		i
 ) => algorithm -> Int -> IO (Double, i)
primesPerformance algorithm	= ToolShed.System.TimePure.getCPUSeconds . (Math.Primes.primes algorithm !!)

-- | Measures the CPU-time required to find the specified number of /Mersenne/-numbers, which is returned together with the requested list.
mersenneNumbersPerformance :: Math.Primes.Algorithmic algorithm => algorithm -> Int -> IO (Double, [Integer])
mersenneNumbersPerformance primalityAlgorithm i
	| i < 0		= error . showString "Factory.Test.Performance.Primes.mersenneNumbersPerformance:\tnegative number; " $ shows i "."
	| otherwise	= ToolShed.System.TimePure.getCPUSeconds . take i $ Math.Primes.mersenneNumbers primalityAlgorithm

