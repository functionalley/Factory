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

 [@DESCRIPTION@]	Defines /QuickCheck/-properties "Math.Power".
-}

module Factory.Test.QuickCheck.Power(
-- * Constants
	results
) where

import qualified	Data.List
import qualified	Factory.Math.Power	as Math.Power
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	Test.QuickCheck.quickCheckResult prop_squaresFrom,
	Test.QuickCheck.quickCheckResult prop_raiseModulo
 ] where
	prop_squaresFrom :: Integer -> Integer -> Test.QuickCheck.Property
	prop_squaresFrom from l	= Test.QuickCheck.label "prop_squaresFrom" . (\(x, y) -> y == Math.Power.square x) . Data.List.genericIndex (Math.Power.squaresFrom from) $ abs l

	prop_raiseModulo :: Integer -> Integer -> Integer -> Test.QuickCheck.Property
	prop_raiseModulo b e m	= m /= 0	==> Test.QuickCheck.label "prop_raiseModulo" $ Math.Power.raiseModulo b e' m == (b ^ e') `mod` m	where
		e' :: Integer
		e'	= abs e


