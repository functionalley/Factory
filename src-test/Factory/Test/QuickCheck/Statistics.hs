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

 [@DESCRIPTION@]	Defines /QuickCheck/-properties for "Math.Statistics".
-}

module Factory.Test.QuickCheck.Statistics(
-- * Constants
	results
) where

import qualified	Data.Array.IArray
import qualified	Data.List
import qualified	Data.Map
import qualified	Data.Numbers.Primes
import qualified	Data.Set
import qualified	Factory.Math.Implementations.Factorial	as Math.Implementations.Factorial
import qualified	Factory.Math.Power			as Math.Power
import qualified	Factory.Math.Statistics			as Math.Statistics
import			Factory.Test.QuickCheck.Factorial()
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	Test.QuickCheck.quickCheckResult prop_nC0,
	Test.QuickCheck.quickCheckResult prop_nC1,
	Test.QuickCheck.quickCheckResult prop_sum,
	Test.QuickCheck.quickCheckResult prop_symmetry,
	Test.QuickCheck.quickCheckResult prop_prime,
	Test.QuickCheck.quickCheckResult prop_nP0,
	Test.QuickCheck.quickCheckResult prop_nP1,
	Test.QuickCheck.quickCheckResult prop_zeroVariance,
	Test.QuickCheck.quickCheckResult prop_zeroAverageAbsoluteDeviation,
	Test.QuickCheck.quickCheckResult prop_balance,
	Test.QuickCheck.quickCheckResult prop_varianceRelocated,
	Test.QuickCheck.quickCheckResult prop_varianceScaled,
	Test.QuickCheck.quickCheckResult prop_varianceOrder,
	Test.QuickCheck.quickCheckResult prop_equivalence,
	Test.QuickCheck.quickCheckResult prop_varianceOfArray,
	Test.QuickCheck.quickCheckResult prop_varianceOfMap,
	Test.QuickCheck.quickCheckResult prop_meanOfSet,
	Test.QuickCheck.quickCheckResult prop_weightedMeanRational,
	Test.QuickCheck.quickCheckResult prop_weightedMeanInteger,
	Test.QuickCheck.quickCheckResult prop_weightedMeanUniformDenominator
 ] where
	prop_nC0, prop_nC1, prop_sum :: Math.Implementations.Factorial.Algorithm -> Integer -> Test.QuickCheck.Property
	prop_nC0 algorithm n	= Test.QuickCheck.label "prop_nC0" $ Math.Statistics.nCr algorithm (abs n) 0 == 1

	prop_nC1 algorithm i	= Test.QuickCheck.label "prop_nC1" $ Math.Statistics.nCr algorithm n 1 == n	where
		n	= succ $ abs i

	prop_sum algorithm i	= Test.QuickCheck.label "prop_sum" $ sum (Math.Statistics.nCr algorithm n `map` [0 .. n]) == 2 ^ n	where
		n	= succ $ abs i

	prop_symmetry, prop_prime :: Math.Implementations.Factorial.Algorithm -> (Integer, Integer) -> Test.QuickCheck.Property
	prop_symmetry algorithm (i, j)	= Test.QuickCheck.label "prop_symmetry" $ Math.Statistics.nCr algorithm n r == Math.Statistics.nCr algorithm n (n - r)	where
		[r, n]		= Data.List.sort $ map abs [i, j]

	prop_prime algorithm (i, j)	= r `notElem` [0, n]	==> Test.QuickCheck.label "prop_prime" $ (Math.Statistics.nCr algorithm n r `mod` n) == 0	where
		n	= Data.Numbers.Primes.primes !! fromIntegral (i `mod` 500000)
		r	= j `mod` n	-- Ensure r is smaller than n.

	prop_nP0, prop_nP1 :: Integer -> Test.QuickCheck.Property
	prop_nP0 n	= Test.QuickCheck.label "prop_nP0" $ Math.Statistics.nPr (abs n) 0 == 1

	prop_nP1 i	= Test.QuickCheck.label "prop_nP1" $ Math.Statistics.nPr n 1 == n	where
		n	= succ $ abs i

	prop_zeroVariance, prop_zeroAverageAbsoluteDeviation :: Rational -> Test.QuickCheck.Property
	prop_zeroVariance x			= Test.QuickCheck.label "prop_zeroVariance" $ Math.Statistics.getVariance (replicate 32 x) == (0 :: Rational)
	prop_zeroAverageAbsoluteDeviation x	= Test.QuickCheck.label "zeroAverageAbsoluteDeviation" $ Math.Statistics.getAverageAbsoluteDeviation (replicate 32 x) == (0 :: Rational)

	prop_balance, prop_varianceRelocated, prop_varianceScaled, prop_varianceOrder, prop_equivalence, prop_varianceOfMap, prop_meanOfSet, prop_varianceOfArray :: [Integer] -> Test.QuickCheck.Property
	prop_balance l			= not (null l)	==> Test.QuickCheck.label "prop_balance" . (== 0) . abs . sum $ map (\i -> toRational i - Math.Statistics.getMean l) l
	prop_varianceRelocated l	= not (null l)	==> Test.QuickCheck.label "prop_varianceRelocated" $ (Math.Statistics.getVariance l :: Rational) == Math.Statistics.getVariance (map succ l)
	prop_varianceScaled l		= not (null l)	==> Test.QuickCheck.label "prop_varianceScaled" $ (4 * Math.Statistics.getVariance l :: Rational) == Math.Statistics.getVariance (map (* 2) l)
	prop_varianceOrder l		= not (null l)	==> Test.QuickCheck.label "prop_varianceOrder" $ Math.Statistics.getVariance l == (Math.Statistics.getVariance (reverse l) :: Rational)
	prop_equivalence l		= not (null l)	==> Test.QuickCheck.label "prop_equivalence" $ Math.Statistics.getVariance l == Math.Statistics.getMean (map Math.Power.square l) - Math.Power.square (Math.Statistics.getMean l :: Rational)
	prop_varianceOfArray l		= not (null l)	==> Test.QuickCheck.label "prop_varianceOfArray" $ Math.Statistics.getVariance (
		Data.Array.IArray.array (1, length l) $ zip [1 ..] l :: Data.Array.IArray.Array Int Integer
	 ) == (Math.Statistics.getVariance l :: Rational)
	prop_varianceOfMap l		= not (null l)	==> Test.QuickCheck.label "prop_varianceOfMap" $ Math.Statistics.getVariance (Data.Map.fromList $ zip [0 :: Int ..] l) == (Math.Statistics.getVariance l :: Rational)
	prop_meanOfSet l		= not (null l')	==> Test.QuickCheck.label "prop_meanOfSet" $ Math.Statistics.getMean (Data.Set.fromList l') == (Math.Statistics.getMean l' :: Rational)	where
		l'	= Data.List.nub l

	prop_weightedMeanRational :: [(Rational, Rational)] -> Test.QuickCheck.Property
	prop_weightedMeanRational assoc	= (denominator /= 0) ==> Test.QuickCheck.label "prop_weightedMeanRational" $ Math.Statistics.getWeightedMean assoc == (
		sum (map (uncurry (*)) assoc) / denominator
	 ) where
		denominator	= sum $ map snd assoc


	prop_weightedMeanInteger :: [(Integer, Integer)] -> Test.QuickCheck.Property
	prop_weightedMeanInteger assoc	= (denominator /= 0) ==> Test.QuickCheck.label "prop_weightedMeanInteger" $ Math.Statistics.getWeightedMean assoc == (
		toRational (
			sum $ map (
				uncurry (*)
			) assoc
		) / toRational denominator
	 ) where
		denominator	= sum $ map snd assoc

	prop_weightedMeanUniformDenominator :: [Rational] -> Integer -> Test.QuickCheck.Property
	prop_weightedMeanUniformDenominator numerators i	= (not (null numerators) && i /= 0) ==> Test.QuickCheck.label "prop_weightedMeanUniformDenominator" $ Math.Statistics.getWeightedMean (
		zip numerators $ repeat i
	 ) == (
		Math.Statistics.getMean numerators :: Rational
	 )

