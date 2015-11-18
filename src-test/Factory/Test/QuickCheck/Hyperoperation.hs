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

 [@DESCRIPTION@]	Defines /QuickCheck/-properties for "Math.Hyperoperation".
-}

module Factory.Test.QuickCheck.Hyperoperation(
-- * Constants
	results
) where

import qualified	Factory.Math.Hyperoperation	as Math.Hyperoperation
import qualified	Test.QuickCheck

type Rank	= Int

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	Test.QuickCheck.quickCheckResult prop_rankCoincides,
	Test.QuickCheck.quickCheckResult prop_baseCoincides,
	Test.QuickCheck.quickCheckResult prop_hyperExponentCoincides,
	Test.QuickCheck.quickCheckResult prop_succ,
	Test.QuickCheck.quickCheckResult prop_addition,
	Test.QuickCheck.quickCheckResult prop_multiplication,
	Test.QuickCheck.quickCheckResult prop_exponentiation
 ] where
	prop_rankCoincides :: Rank -> Test.QuickCheck.Property
	prop_rankCoincides rank = Test.QuickCheck.label "prop_rankCoincides" $ Math.Hyperoperation.hyperoperation rank' 2 2 == 4	where
		rank' :: Rank
		rank'	= succ $ rank `mod` 1000

	prop_baseCoincides :: Rank -> Integer -> Test.QuickCheck.Property
	prop_baseCoincides rank base	= Test.QuickCheck.label "prop_baseCoincides" $ Math.Hyperoperation.hyperoperation rank' base 1 == base	where
		rank' :: Rank
		rank'	= 2 + (rank `mod` 1000)

	prop_hyperExponentCoincides :: Rank -> Integer -> Test.QuickCheck.Property
	prop_hyperExponentCoincides rank hyperExponent	= Test.QuickCheck.label "prop_hyperExponentCoincides" $ Math.Hyperoperation.hyperoperation rank' 1 hyperExponent' == 1	where
		rank' :: Rank
		rank'	= 3 + (rank `mod` 1000)

		hyperExponent' :: Math.Hyperoperation.HyperExponent
		hyperExponent'	= abs hyperExponent

	prop_succ, prop_addition, prop_multiplication, prop_exponentiation :: Integer -> Integer -> Test.QuickCheck.Property
	prop_succ base hyperExponent			= Test.QuickCheck.label "prop_succ" $ Math.Hyperoperation.hyperoperation Math.Hyperoperation.succession base hyperExponent' == succ (fromIntegral hyperExponent')	where
		hyperExponent' :: Math.Hyperoperation.HyperExponent
		hyperExponent'	= abs hyperExponent

	prop_addition base hyperExponent		= Test.QuickCheck.label "prop_addition" $ Math.Hyperoperation.hyperoperation Math.Hyperoperation.addition base hyperExponent' == base + fromIntegral hyperExponent'	where
		hyperExponent' :: Math.Hyperoperation.HyperExponent
		hyperExponent'	= abs hyperExponent

	prop_multiplication base hyperExponent		= Test.QuickCheck.label "prop_multiplication" $ Math.Hyperoperation.hyperoperation Math.Hyperoperation.multiplication base hyperExponent' == base * fromIntegral hyperExponent'	where
		hyperExponent' :: Math.Hyperoperation.HyperExponent
		hyperExponent'	= abs hyperExponent

	prop_exponentiation base hyperExponent		= Test.QuickCheck.label "prop_exponentiation" $ Math.Hyperoperation.hyperoperation Math.Hyperoperation.exponentiation base hyperExponent' == base ^ hyperExponent'	where
		hyperExponent' :: Math.Hyperoperation.HyperExponent
		hyperExponent'	= abs hyperExponent


