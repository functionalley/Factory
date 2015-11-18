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

 [@DESCRIPTION@]	Times functions exported from module "Math.Hyperoperation".
-}

module Factory.Test.Performance.Hyperoperation(
-- * Functions
	hyperoperationPerformance,
	hyperoperationPerformanceGraphRank,
	hyperoperationPerformanceGraphExponent
) where

import qualified	Factory.Math.Hyperoperation	as Math.Hyperoperation
import qualified	ToolShed.System.TimePure

-- | Measures the CPU-time required by 'Math.Hyperoperation.hyperoperation'.
hyperoperationPerformance :: (Integral rank, Show rank) => rank -> Math.Hyperoperation.Base -> Math.Hyperoperation.HyperExponent -> IO (Double, Integer)
hyperoperationPerformance rank base	= ToolShed.System.TimePure.getCPUSeconds . Math.Hyperoperation.hyperoperation rank base

{- |
	* Measure the CPU-time required by 'Math.Hyperoperation.hyperoperation', against a linearly increasing /rank/.

	* CAVEAT: nothing is returned, since the result is printed ... and it never terminates.
-}
hyperoperationPerformanceGraphRank
	:: Bool	-- ^ Verbose.
	-> Math.Hyperoperation.Base
	-> Math.Hyperoperation.HyperExponent
	-> IO ()
hyperoperationPerformanceGraphRank verbose base hyperExponent	= mapM_ (
	\rank	-> hyperoperationPerformance rank base hyperExponent >>= putStrLn . shows rank . showChar '\t' . (
		if verbose
			then (`shows` "")
			else (`shows` "") . fst
	)
 ) [0 :: Int ..]

{- |
	* Measure the CPU-time required by 'Math.Hyperoperation.hyperoperation', against a linearly increasing /hyper-exponent/.

	* CAVEAT: nothing is returned, since the result is printed ... and it never terminates.
-}
hyperoperationPerformanceGraphExponent :: (Integral rank, Show rank)
	=> Bool	-- ^ Verbose.
	-> rank
	-> Math.Hyperoperation.Base
	-> IO ()
hyperoperationPerformanceGraphExponent verbose rank base	= mapM_ (
	\hyperExponent	-> hyperoperationPerformance rank base hyperExponent >>= putStrLn . shows hyperExponent . showChar '\t' . (
		if verbose
			then (`shows` "")
			else (`shows` "") . fst
	)
 ) [0 ..]
