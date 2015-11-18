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

 [@DESCRIPTION@]	Defines /QuickCheck/-properties for "Math.PerfectPower".
-}

module Factory.Test.QuickCheck.PerfectPower(
-- * Constants
	results
) where

import qualified	Data.Maybe
import qualified	Factory.Math.PerfectPower	as Math.PerfectPower
import qualified	Factory.Math.Power		as Math.Power
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	Test.QuickCheck.quickCheckResult prop_maybeSquareNumber,
	Test.QuickCheck.quickCheckResult prop_rewriteRule,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 10000 } prop_notSquare,
	Test.QuickCheck.quickCheckResult prop_isPerfectPower
 ] where
	prop_maybeSquareNumber, prop_notSquare, prop_rewriteRule :: Integer -> Test.QuickCheck.Property
	prop_maybeSquareNumber i	= Test.QuickCheck.label "prop_maybeSquareNumber" $ Math.PerfectPower.maybeSquareNumber (Math.Power.square i) == Just (abs i)

	prop_notSquare i	= abs i > 0	==> Test.QuickCheck.label "prop_notSquare" . Data.Maybe.isNothing $ Math.PerfectPower.maybeSquareNumber (succ $ i ^ (10 {-promote rounding-error using big number-} :: Int))

	prop_rewriteRule i	= Test.QuickCheck.label "prop_rewriteRule" $ Math.PerfectPower.isPerfectPower i' == Math.PerfectPower.isPerfectPower (fromIntegral i' :: Int)	where
		i'	= abs i

	prop_isPerfectPower :: Integer -> Integer -> Test.QuickCheck.Property
	prop_isPerfectPower b e	= Test.QuickCheck.label "prop_isPerfectPower" . Math.PerfectPower.isPerfectPower $ b' ^ e'	where
		b'	= 2 + (b `mod` 10)
		e'	= 2 + (e `mod` 8)


