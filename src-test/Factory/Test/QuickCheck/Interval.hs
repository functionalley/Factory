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

 [@DESCRIPTION@]	Defines /QuickCheck/-properties for "Data.Interval".
-}

module Factory.Test.QuickCheck.Interval(
-- * Constants
	results
) where

import qualified	Data.Ratio
import qualified	Factory.Data.Interval	as Data.Interval
import qualified	Test.QuickCheck

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 1000 } `mapM` [prop_product]	where
	prop_product :: Data.Ratio.Ratio Integer -> Integer -> Data.Interval.Interval Integer -> Test.QuickCheck.Property
	prop_product ratio minLength interval	= Test.QuickCheck.label "prop_product" $ Data.Interval.product' ratio' minLength' interval' == product (Data.Interval.toList interval')	where
		interval'	= Data.Interval.normalise interval
		minLength'	= succ $ minLength `mod` 1000
		ratio'
			| r > 1		= recip r
			| otherwise	= r
			where
				r	= abs ratio
