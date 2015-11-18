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

 [@DESCRIPTION@]	Defines /QuickCheck/-properties for "Math.Radix".
-}

module Factory.Test.QuickCheck.Radix(
-- * Constants
	results,
-- * Types
-- ** Type-synonyms
--	Testable
) where

import qualified	Factory.Math.Radix	as Math.Radix
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))

type Testable	= (Int, Integer) -> Test.QuickCheck.Property

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= mapM Test.QuickCheck.quickCheckResult [prop_reversable, prop_digitalRoot]	where
	prop_reversable, prop_digitalRoot :: Testable
	prop_reversable (b, n)	= abs base > 1 ==> Test.QuickCheck.label "prop_reversable" $ Math.Radix.fromBase base (Math.Radix.toBase base n) == n	where
		base	= (b `mod` 73) - 36

	prop_digitalRoot (_, n)	= Test.QuickCheck.label "prop_digitalRoot" $ Math.Radix.digitalRoot n' == 9	where
		n'	= 9 * succ (abs n)

