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

 [@DESCRIPTION@]	Functions for probability-distributions.

 [@CAVEAT@]	Because data-constructors are exposed, 'ToolShed.SelfValidate.isValid' need not be called.
-}

module Factory.Math.Probability(
-- * Type-classes
	Distribution(..),
-- * Types
-- ** Data-types
	ContinuousDistribution(..),
	DiscreteDistribution(..),
-- * Functions
	maxPreciseInteger,
--	minPositiveFloat,
	boxMullerTransform,
--	reProfile,
	generateStandardizedNormalDistribution,
	generateContinuousPopulation,
--	generatePoissonDistribution,
	generateDiscretePopulation
) where

import qualified	Control.Arrow
import			Control.Arrow((***), (&&&))
import qualified	Factory.Data.Interval	as Data.Interval
import qualified	Factory.Math.Power	as Math.Power
import qualified	System.Random
import qualified	ToolShed.Data.List
import qualified	ToolShed.Data.Pair
import qualified	ToolShed.SelfValidate

-- | The maximum integer which can be accurately represented as a Double.
maxPreciseInteger  :: RealFloat a => a -> Integer
maxPreciseInteger	= (2 ^) . floatDigits

{- |
	* Determines the minimum positive floating-point number, which can be represented by using the parameter's type.

	* Only the type of the parameter is relevant, not its value.
-}
minPositiveFloat :: RealFloat a => a -> a
minPositiveFloat	= encodeFloat 1 . uncurry (-) . (fst . floatRange &&& floatDigits)

-- | Describes /continuous probability-distributions/; <http://en.wikipedia.org/wiki/List_of_probability_distributions#Continuous_distributions>.
data ContinuousDistribution parameter
	= ExponentialDistribution parameter {-lambda-}				-- ^ Defines an /Exponential/-distribution with a particular /lambda/; <http://en.wikipedia.org/wiki/Exponential_distribution>.
	| LogNormalDistribution parameter {-location-} parameter {-scale2-}	-- ^ Defines a distribution whose logarithm is normally distributed with a particular /mean/ & /variance/; <http://en.wikipedia.org/wiki/Lognormal>.
	| NormalDistribution parameter {-mean-} parameter {-variance-}		-- ^ Defines a /Normal/-distribution with a particular /mean/ & /variance/; <http://en.wikipedia.org/wiki/Normal_distribution>.
	| UniformDistribution (Data.Interval.Interval parameter)		-- ^ Defines a /Uniform/-distribution within a /closed interval/; <http://en.wikipedia.org/wiki/Uniform_distribution>.
	deriving (Eq, Read, Show)

instance (Floating parameter, Ord parameter, Show parameter) => ToolShed.SelfValidate.SelfValidator (ContinuousDistribution parameter)	where
	getErrors probabilityDistribution	= ToolShed.SelfValidate.extractErrors $ case probabilityDistribution of
		ExponentialDistribution lambda		-> [(lambda <= 0, "'lambda' must exceed zero; " ++ show probabilityDistribution ++ ".")]
		LogNormalDistribution location scale2	-> let
			maxParameter	= log . fromInteger $ maxPreciseInteger (undefined :: Double)
		 in [
			(scale2 <= 0,						"'scale' must exceed zero; " ++ show probabilityDistribution ++ "."),
			(location > maxParameter || scale2 > maxParameter,	"loss of precision will result from either 'location' or 'scale^2' exceeding '" ++ show maxParameter ++ "'; " ++ show probabilityDistribution ++ ".")
		 ]
		NormalDistribution _ variance		-> [(variance <= 0, "variance must exceed zero; " ++ show probabilityDistribution ++ ".")]
		UniformDistribution interval		-> [(Data.Interval.isReversed interval, "reversed interval='" ++ show probabilityDistribution ++ "'.")]

-- | Describes /discrete probability-distributions/; <http://en.wikipedia.org/wiki/List_of_probability_distributions#Discrete_distributions>.
data DiscreteDistribution parameter
	= PoissonDistribution parameter {-lambda-}			-- ^ Defines an /Poisson/-distribution with a particular /lambda/; <http://en.wikipedia.org/wiki/Poisson_distribution>.
	| ShiftedGeometricDistribution parameter {-probability-}	-- ^ Defines an /Geometric/-distribution with a particular probability of success; <http://en.wikipedia.org/wiki/Geometric_distribution>.
	deriving (Eq, Read, Show)

instance (Num parameter, Ord parameter, Show parameter) => ToolShed.SelfValidate.SelfValidator (DiscreteDistribution parameter)	where
	getErrors probabilityDistribution	= ToolShed.SelfValidate.extractErrors $ case probabilityDistribution of
		PoissonDistribution lambda			-> [(lambda <= 0, "'lambda' must exceed zero; " ++ show probabilityDistribution ++ ".")]
		ShiftedGeometricDistribution probability	-> [(any ($ probability) [(<= 0), (> 1)], "probability must be in the semi-closed unit-interval (0, 1]; " ++ show probabilityDistribution ++ ".")]

-- | Defines a common interface for probability-distributions.
class Distribution probabilityDistribution	where
	generatePopulation
		:: (Fractional sample, System.Random.RandomGen randomGen)
		=> probabilityDistribution
		-> randomGen	-- ^ A generator of /uniformly distributed/ random numbers.
		-> [sample]	-- ^ CAVEAT: the integers generated for discrete distributions are represented by a fractional type; use 'generateDiscretePopulation' if this is a problem.

	getMean :: Fractional mean => probabilityDistribution -> mean	-- ^ The theoretical mean.

	getStandardDeviation :: Floating standardDeviation => probabilityDistribution -> standardDeviation-- ^ The theoretical standard-deviation.
	getStandardDeviation	= sqrt . getVariance	-- Default implementation.

	getVariance :: Floating variance => probabilityDistribution -> variance	-- ^ The theoretical variance.
	getVariance	= Math.Power.square . getStandardDeviation	-- Default implementation.

instance (RealFloat parameter, Show parameter, System.Random.Random parameter) => Distribution (ContinuousDistribution parameter)	where
	generatePopulation probabilityDistribution	= map realToFrac {-parameter -> sample-} . generateContinuousPopulation probabilityDistribution

	getMean (ExponentialDistribution lambda)			= realToFrac $ recip lambda
	getMean (LogNormalDistribution location scale2)			= realToFrac . exp . (+ location) $ scale2 / 2
	getMean (NormalDistribution mean _)				= realToFrac mean
	getMean (UniformDistribution (minParameter, maxParameter))	= realToFrac $ (minParameter + maxParameter) / 2

	getVariance (ExponentialDistribution lambda)			= realToFrac . recip $ Math.Power.square lambda
	getVariance (LogNormalDistribution location scale2)		= realToFrac $ (exp scale2 - 1) * exp (2 * location + scale2)	-- N.B.: for standard-deviation == mean, use scale^2 == ln 2.
	getVariance (NormalDistribution _ variance)			= realToFrac variance
	getVariance (UniformDistribution (minParameter, maxParameter))	= realToFrac $ Math.Power.square (maxParameter - minParameter) / 12

instance (RealFloat parameter, Show parameter, System.Random.Random parameter) => Distribution (DiscreteDistribution parameter)	where
	generatePopulation probabilityDistribution		= map fromInteger . generateDiscretePopulation probabilityDistribution

	getMean (PoissonDistribution lambda)			= realToFrac lambda
	getMean (ShiftedGeometricDistribution probability)	= realToFrac $ recip probability

	getVariance (PoissonDistribution lambda)		= realToFrac lambda
	getVariance (ShiftedGeometricDistribution probability)	= realToFrac $ (1 - probability) / Math.Power.square probability

{- |
	* Converts a pair of independent /uniformly distributed/ random numbers, within the /semi-closed unit interval/ /(0,1]/,
	to a pair of independent /normally distributed/ random numbers, of standardized /mean/=0, and /variance/=1.

	* <http://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform>.
-}
boxMullerTransform :: (
	Floating	f,
	Ord		f,
	Show		f
 )
	=> (f, f)	-- ^ Independent, /uniformly distributed/ random numbers, which must be within the /semi-closed unit interval/, /(0,1]/.
	-> (f, f)	-- ^ Independent, /normally distributed/ random numbers, with standardized /mean/=0 and /variance/=1.
boxMullerTransform cartesian
	| not . uncurry (&&) $ ToolShed.Data.Pair.mirror inSemiClosedUnitInterval cartesian	= error $ "Factory.Math.Probability.boxMullerTransform:\tspecified Cartesian coordinates, must be within semi-closed unit-interval (0, 1]; " ++ show cartesian
	| otherwise										= polarToCartesianTransform $ (sqrt . negate . (* 2) . log *** (* 2) . (* pi)) cartesian
	where
		inSemiClosedUnitInterval :: (Num n, Ord n) => n -> Bool
		inSemiClosedUnitInterval	= uncurry (&&) . ((> 0) &&& (<= 1))

		polarToCartesianTransform :: Floating f => (f, f) -> (f, f)
		polarToCartesianTransform	= uncurry (*) . Control.Arrow.second cos &&& uncurry (*) . Control.Arrow.second sin

{- |
	* Uses the supplied random-number generator,
	to generate a conceptually infinite list, of /normally distributed/ random numbers, with standardized /mean/=0, and /variance/=1.

	* <http://en.wikipedia.org/wiki/Normal_distribution>, <http://mathworld.wolfram.com/NormalDistribution.html>.
-}
generateStandardizedNormalDistribution :: (
	RealFloat		f,
	Show			f,
	System.Random.Random	f,
	System.Random.RandomGen	randomGen
 ) => randomGen -> [f]
generateStandardizedNormalDistribution	= ToolShed.Data.List.linearise . uncurry (zipWith $ curry boxMullerTransform) . ToolShed.Data.Pair.mirror (
	System.Random.randomRs (minPositiveFloat undefined, 1)
 ) . System.Random.split

-- | Stretches and shifts a /distribution/ to achieve the required /mean/ and /standard-deviation/.
reProfile :: (Distribution distribution, Floating n) => distribution -> [n] -> [n]
reProfile distribution	= map ((+ getMean distribution) . (* getStandardDeviation distribution))

-- | Uses the supplied random-number generator, to generate a conceptually infinite population, with the specified continuous probability-distribution.
generateContinuousPopulation :: (
	RealFloat		f,
	Show			f,
	System.Random.Random	f,
	System.Random.RandomGen	randomGen
 )
	=> ContinuousDistribution f
	-> randomGen	-- ^ A generator of /uniformly distributed/ random numbers.
	-> [f]
generateContinuousPopulation probabilityDistribution randomGen
	| not $ ToolShed.SelfValidate.isValid probabilityDistribution	= error $ "Factory.Math.Probability.generateContinuousPopulation:\t" ++ ToolShed.SelfValidate.getFirstError probabilityDistribution
	| otherwise							= (
		case probabilityDistribution of
			ExponentialDistribution lambda		-> let
				quantile	= (/ lambda) . negate . log . (1 -)	-- <http://en.wikipedia.org/wiki/Quantile_function>.
			 in map quantile . System.Random.randomRs (0, 1)
			LogNormalDistribution location scale2	-> map (
				exp . (+ location) . (* sqrt scale2)	-- Stretch the standard-deviation & re-locate the mean to that specified for the log-space, then return to the original coordinates.
			 ) . generateStandardizedNormalDistribution
			NormalDistribution _ _			-> reProfile probabilityDistribution . generateStandardizedNormalDistribution
			UniformDistribution interval		-> System.Random.randomRs interval
	) randomGen

{- |
	* Uses the supplied random-number generator,
	to generate a conceptually infinite population, of random integers conforming to the /Poisson distribution/; <http://en.wikipedia.org/wiki/Poisson_distribution>.

	* CAVEAT:
		uses an algorithm by Knuth, which having a /linear time-complexity/ in /lambda/, can be intolerably slow;
		also, the term @exp $ negate lambda@, underflows for large /lambda/;
		so for large /lambda/, this implementation returns the appropriate 'NormalDistribution'.
-}
generatePoissonDistribution :: (
	Integral		sample,
	RealFloat		lambda,
	Show			lambda,
	System.Random.Random	lambda,
	System.Random.RandomGen	randomGen
 )
	=> lambda	-- ^ Defines the required approximate value of both /mean/ and /variance/.
	-> randomGen
	-> [sample]
generatePoissonDistribution lambda
	| lambda <= 0	= error $ "Factory.Math.Probability.generatePoissonDistribution:\tlambda must exceed zero " ++ show lambda
	| lambda > (
		negate . log $ minPositiveFloat lambda	-- Guard against underflow, in the user-defined type for lambda.
	)		= filter (>= 0) . map round . (reProfile (PoissonDistribution lambda) :: [Double] -> [Double]) . generateStandardizedNormalDistribution
	| otherwise	= generator
	where
		generator	= uncurry (:) . (
			fst . head . dropWhile (
				(> exp (negate lambda)) . snd	-- CAVEAT: underflows if lambda > (103 :: Float, 745 :: Double).
			) . scanl (
				\accumulator random	-> succ *** (* random) $ accumulator
			) (negate 1, 1) . System.Random.randomRs (0, 1) *** generator {-recurse-}
		 ) . System.Random.split

-- | Uses the supplied random-number generator, to generate a conceptually infinite population, with the specified discrete probability-distribution.
generateDiscretePopulation :: (
	Integral		sample,
	Ord			parameter,
	RealFloat		parameter,
	Show			parameter,
	System.Random.Random	parameter,
	System.Random.RandomGen	randomGen
 )
	=> DiscreteDistribution parameter
	-> randomGen	-- ^ A generator of /uniformly distributed/ random numbers.
	-> [sample]
generateDiscretePopulation probabilityDistribution randomGen
	| not $ ToolShed.SelfValidate.isValid probabilityDistribution	= error $ "Factory.Math.Probability.generateDiscretePopulation:\t" ++ ToolShed.SelfValidate.getFirstError probabilityDistribution
	| otherwise							= (
		case probabilityDistribution of
			PoissonDistribution lambda	-> generatePoissonDistribution lambda
			ShiftedGeometricDistribution probability
				| probability == 1	-> const $ repeat 1	-- The first Bernoulli Trial is guaranteed to succeed.
				| otherwise		-> map ceiling {-minimum 1-} . (\x -> x :: [Rational]) . generatePopulation (ExponentialDistribution . negate $ log (1 - probability))	-- The geometric distribution is a discrete version of the exponential distribution.
	) randomGen

