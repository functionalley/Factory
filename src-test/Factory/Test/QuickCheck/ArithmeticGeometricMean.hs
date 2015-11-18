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

 [@DESCRIPTION@]	Defines /QuickCheck/-properties for "Math.ArithmeticGeometricMean".
-}

module Factory.Test.QuickCheck.ArithmeticGeometricMean(
-- * Constants
	results,
-- * Types
-- ** Type-synonyms
--	Testable
) where

import qualified	Data.Tuple
import qualified	Factory.Math.ArithmeticGeometricMean	as Math.ArithmeticGeometricMean
import qualified	Factory.Math.Implementations.SquareRoot	as Math.Implementations.SquareRoot
import qualified	Factory.Math.Precision			as Math.Precision
import			Factory.Test.QuickCheck.SquareRoot()
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))

type Testable	= Math.Implementations.SquareRoot.Algorithm -> Math.Precision.DecimalDigits -> Math.ArithmeticGeometricMean.AGM -> Int -> Test.QuickCheck.Property

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= mapM Test.QuickCheck.quickCheckResult [prop_symmetrical, prop_bounds]	where
	prop_symmetrical, prop_bounds :: Testable
	prop_symmetrical squareRootAlgorithm decimalDigits agm index	= Math.ArithmeticGeometricMean.isValid agm ==> Test.QuickCheck.label "prop_symmetrical" . and . tail . take index' $ zipWith (==) (
		Math.ArithmeticGeometricMean.convergeToAGM squareRootAlgorithm decimalDigits' agm
	 ) (
		Math.ArithmeticGeometricMean.convergeToAGM squareRootAlgorithm decimalDigits' $ Data.Tuple.swap agm
	 ) where
		decimalDigits'	= succ $ decimalDigits `mod` 64
		index'		= succ $ index `mod` 8

	prop_bounds squareRootAlgorithm decimalDigits agm index	= all ($ agm) [Math.ArithmeticGeometricMean.isValid, uncurry (/=)] ==> Test.QuickCheck.label "prop_bounds" . all (uncurry (>=)) . tail . take index' $ Math.ArithmeticGeometricMean.convergeToAGM squareRootAlgorithm decimalDigits' agm
		where
			decimalDigits'	= 33 {-test is sensitive to rounding-errors-} + (decimalDigits `mod` 96)
			index'		= succ $ index `mod` 5

