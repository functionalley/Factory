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

 [@DESCRIPTION@]	Times the functions exported from module "Math.Statistics".
-}

module Factory.Test.Performance.Statistics(
-- * Functions
	nCrPerformance
) where

import qualified	Control.DeepSeq
import qualified	Factory.Math.Factorial	as Math.Factorial
import qualified	Factory.Math.Statistics	as Math.Statistics
import qualified	ToolShed.System.TimePure

-- | Measures the CPU-time required by 'Math.Statistics.nCr'.
nCrPerformance :: (
	Control.DeepSeq.NFData		i,
	Integral			i,
	Math.Factorial.Algorithmic	factorialAlgorithm,
	Show				i
 )
	=> factorialAlgorithm
	-> i	-- ^ The total number from which to select.
	-> i	-- ^ The number of items in a sample.
	-> IO (Double, i)
nCrPerformance factorialAlgorithm n r	= ToolShed.System.TimePure.getCPUSeconds $ Math.Statistics.nCr factorialAlgorithm n r

