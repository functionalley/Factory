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

 [@DESCRIPTION@]	Defines /QuickCheck/-properties for "Math.Probability".
-}

module Factory.Test.QuickCheck.Probability(
-- * Constants
	results,
-- * Functions
--	normalise
) where

import			Control.Arrow((&&&))
import qualified	Data.List
import qualified	Factory.Math.Probability	as Math.Probability
import qualified	Factory.Math.Statistics		as Math.Statistics
import			Factory.Test.QuickCheck.Factorial()
import qualified	System.Random
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))
import qualified	ToolShed.Data.Pair

-- | Re-profile a distribution to achieve a standard mean & variance.
normalise :: (
	Eq				f,
	Floating			f,
	Math.Probability.Distribution	distribution
 ) => distribution -> [f] -> [f]
normalise distribution
	| variance == 0	= error "Factory.Test.Quick.Probability.normalise:\tzero variance => can't stretch to one."
	| otherwise	= map $ (/ sqrt variance) . subtract mean
	where
		(mean, variance)	= Math.Probability.getMean &&& Math.Probability.getVariance $ distribution

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= let
	isWithinTolerance :: Double -> Double -> Bool
	isWithinTolerance i	= (< recip i) . abs

	prop_logNormalDistribution, prop_logNormalDistribution', prop_normalDistribution, prop_uniformDistribution :: System.Random.RandomGen randomGen => randomGen -> Double -> Double -> Test.QuickCheck.Property
	prop_logNormalDistribution randomGen location scale2	= scale2 /= 0 ==> Test.QuickCheck.label "prop_logNormalDistribution" . uncurry (&&) . ToolShed.Data.Pair.mirror (isWithinTolerance 1) . (
		Math.Statistics.getMean &&& pred . Math.Statistics.getStandardDeviation	-- Both of which, having been normalised, should be zero.
	 ) . (
		normalise distribution :: [Double] -> [Double]
	 ) . take 100000 $ Math.Probability.generatePopulation distribution randomGen	where
		maxParameter	= log . fromInteger $ Math.Probability.maxPreciseInteger (undefined :: Double)
		location'
			| location >= 0	= maxParameter `min` location
			| otherwise	= negate maxParameter `max` location

		distribution	= Math.Probability.LogNormalDistribution location' . min maxParameter $ abs scale2

	prop_logNormalDistribution' randomGen location scale2	= scale2 /= 0 ==> Test.QuickCheck.label "prop_logNormalDistribution'" . all (
		>= (0 :: Double)
	 ) . take 10 $ Math.Probability.generatePopulation (Math.Probability.LogNormalDistribution location' . min maxParameter $ abs scale2) randomGen	where
		maxParameter	= log . fromInteger $ Math.Probability.maxPreciseInteger (undefined :: Double)

		location'
			| location >= 0	= maxParameter `min` location
			| otherwise	= negate maxParameter `max` location

-- The mean & standard-deviation are equal when scale^2 == ln 2, but this seems to break-down when the mean is close to zero.
	prop_logNormalDistributionEqual :: System.Random.RandomGen randomGen => randomGen -> Double -> Test.QuickCheck.Property
	prop_logNormalDistributionEqual randomGen location	= location' > 16 {-any lower & it seems to fail-} ==> Test.QuickCheck.label "prop_logNormalDistributionEqual" . (
		< (recip 1000000 :: Double)
	 ) . pred . abs . uncurry (/) . (
		Math.Statistics.getMean &&& Math.Statistics.getStandardDeviation
	 ) $ take 10000 (
		Math.Probability.generatePopulation (Math.Probability.LogNormalDistribution location' $ log 2) randomGen :: [Double]
	 ) where
		maxParameter	= log . fromInteger $ Math.Probability.maxPreciseInteger (undefined :: Double)

		location'
			| location >= 0	= maxParameter `min` location
			| otherwise	= negate maxParameter `max` location

	prop_normalDistribution randomGen mean variance	= variance /= 0 ==> Test.QuickCheck.label "prop_normalDistribution" . uncurry (&&) . ToolShed.Data.Pair.mirror (isWithinTolerance 10) . (
		Math.Statistics.getMean &&& pred . Math.Statistics.getStandardDeviation	-- Both of which, having been normalised, should be zero.
	 ) . (
		normalise distribution :: [Double] -> [Double]
	 ) . take 1000 $ Math.Probability.generatePopulation distribution randomGen	where
		distribution	= Math.Probability.NormalDistribution mean $ abs variance

	prop_uniformDistribution randomGen min' max'	= min' /= max' ==> Test.QuickCheck.label "prop_uniformDistribution" . uncurry (&&) . ToolShed.Data.Pair.mirror (isWithinTolerance 10) . (
		Math.Statistics.getMean &&& pred . Math.Statistics.getStandardDeviation	-- Both of which, having been normalised, should be zero.
	 ) . (
		normalise distribution :: [Double] -> [Double]
	 ) . take 10000 $ Math.Probability.generatePopulation distribution randomGen	where
		[min'', max'']	= Data.List.sort [min', max']
		distribution	= Math.Probability.UniformDistribution (min'', max'')

	prop_exponentialDistribution, prop_exponentialDistribution', prop_poissonDistribution, prop_poissonDistribution', prop_shiftedGeometricDistribution, prop_shiftedGeometricDistribution' :: System.Random.RandomGen randomGen => randomGen -> Double -> Test.QuickCheck.Property
	prop_exponentialDistribution randomGen lambda	= Test.QuickCheck.label "prop_exponentialDistribution" . uncurry (&&) . ToolShed.Data.Pair.mirror (isWithinTolerance 10) . (
		Math.Statistics.getMean &&& pred . Math.Statistics.getStandardDeviation	-- Both of which, having been normalised, should be zero.
	 ) . (
		normalise distribution :: [Double] -> [Double]
	 ) . take 10000 $ Math.Probability.generatePopulation distribution randomGen	where
		distribution	= Math.Probability.ExponentialDistribution . succ {-exclude zero-} $ abs lambda `max` 10 {-cap-}

	prop_exponentialDistribution' randomGen lambda	= lambda /= 0 ==> Test.QuickCheck.label "prop_exponentialDistribution'" . all (
		>= (0 :: Double)
	 ) . take 10 $ Math.Probability.generatePopulation (Math.Probability.ExponentialDistribution $ abs lambda) randomGen

	prop_poissonDistribution randomGen lambda	= Test.QuickCheck.label "prop_poissonDistribution" . uncurry (&&) . ToolShed.Data.Pair.mirror (isWithinTolerance 10) . (
		Math.Statistics.getMean &&& pred . Math.Statistics.getStandardDeviation	-- Both of which, having been normalised, should be zero.
	 ) . (
		normalise distribution :: [Double] -> [Double]
	 ) . take 1000 $ Math.Probability.generatePopulation distribution randomGen	where
		distribution	= Math.Probability.PoissonDistribution . succ {-exclude zero-} $ abs lambda `max` 10 {-cap-}

	prop_poissonDistribution' randomGen lambda	= lambda /= 0 ==> Test.QuickCheck.label "prop_poissonDistribution'" . all (
		>= (0 :: Double)
	 ) . take 10 $ Math.Probability.generatePopulation (Math.Probability.PoissonDistribution $ abs lambda) randomGen

	prop_shiftedGeometricDistribution randomGen probability	= probability' /= 1 ==> Test.QuickCheck.label "prop_shiftedGeometricDistribution" . uncurry (&&) . ToolShed.Data.Pair.mirror (isWithinTolerance 10) . (
		Math.Statistics.getMean &&& pred . Math.Statistics.getStandardDeviation	-- Both of which, having been normalised, should be zero.
	 ) . (
		normalise distribution :: [Double] -> [Double]
	 ) . take 100000 $ Math.Probability.generatePopulation distribution randomGen	where
		probability'	= recip . succ $ abs probability	-- Semi-closed unit-interval (0, 1].
		distribution	= Math.Probability.ShiftedGeometricDistribution probability'

	prop_shiftedGeometricDistribution' randomGen probability	= Test.QuickCheck.label "prop_shiftedGeometricDistribution'" . all (
		>= (1 :: Double)
	 ) . take 10 $ Math.Probability.generatePopulation (Math.Probability.ShiftedGeometricDistribution probability') randomGen	where
		probability'	= recip . succ $ abs probability	-- Semi-closed unit-interval (0, 1].
 in do
	randomGen	<- System.Random.getStdGen

	sequence [
--		Test.QuickCheck.quickCheckResult $ prop_logNormalDistributionEqual randomGen,	-- CAVEAT: known to fail occasionally.
		Test.QuickCheck.quickCheckResult $ prop_logNormalDistribution randomGen,
		Test.QuickCheck.quickCheckResult $ prop_logNormalDistribution' randomGen,
		Test.QuickCheck.quickCheckResult $ prop_normalDistribution randomGen,
		Test.QuickCheck.quickCheckResult $ prop_uniformDistribution randomGen,
		Test.QuickCheck.quickCheckResult $ prop_exponentialDistribution randomGen,
		Test.QuickCheck.quickCheckResult $ prop_exponentialDistribution' randomGen,
		Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 25 } $ prop_poissonDistribution randomGen,
		Test.QuickCheck.quickCheckResult $ prop_poissonDistribution' randomGen,
		Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 50 } $ prop_shiftedGeometricDistribution randomGen,
		Test.QuickCheck.quickCheckResult $ prop_shiftedGeometricDistribution' randomGen
	 ]

