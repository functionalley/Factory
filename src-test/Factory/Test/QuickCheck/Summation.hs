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

 [@DESCRIPTION@]	Defines /QuickCheck/-properties for "Math.Summation".
-}

module Factory.Test.QuickCheck.Summation(
-- * Constants
	results
) where

import qualified	Factory.Math.Summation	as Math.Summation
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= mapM Test.QuickCheck.quickCheckResult [prop_sum, prop_sumR]	where
	prop_sum, prop_sumR :: Int -> [Rational] -> Test.QuickCheck.Property
	prop_sum chunkSize l	= not (null l)	==> Test.QuickCheck.label "prop_sum" $ Math.Summation.sum' chunkSize' l == sum l	where
		chunkSize'	= 2 + (chunkSize `mod` length l)

	prop_sumR chunkSize l	= not (null l)	==> Test.QuickCheck.label "prop_sumR" $ Math.Summation.sumR chunkSize' l == sum l	where
		chunkSize'	= 2 + (chunkSize `mod` length l)


