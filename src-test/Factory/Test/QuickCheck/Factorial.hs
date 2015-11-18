{-# OPTIONS_GHC -fno-warn-orphans #-}
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

 [@DESCRIPTION@]	Defines /QuickCheck/-properties for "Math.Implementations.Factorial".
-}

module Factory.Test.QuickCheck.Factorial(
-- * Constants
	results,
-- * Types
-- ** Type-synonyms
--	Testable
) where

import			Data.Ratio((%))
import qualified	Factory.Math.Factorial			as Math.Factorial
import qualified	Factory.Math.Implementations.Factorial	as Math.Implementations.Factorial
import			Factory.Math.Implementations.Factorial((!/!))
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))

instance Test.QuickCheck.Arbitrary Math.Implementations.Factorial.Algorithm	where
	arbitrary	= Test.QuickCheck.elements [Math.Implementations.Factorial.Bisection, Math.Implementations.Factorial.PrimeFactorisation]

type Testable	= Integer -> Integer -> Test.QuickCheck.Property

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	Test.QuickCheck.quickCheckResult prop_equivalence,
	Test.QuickCheck.quickCheckResult prop_symmetry,
	Test.QuickCheck.quickCheckResult prop_x0,
	Test.QuickCheck.quickCheckResult prop_0n,
	Test.QuickCheck.quickCheckResult prop_ratio,
	Test.QuickCheck.quickCheckResult prop_consistency
 ] where
	prop_equivalence, prop_symmetry, prop_x0, prop_0n :: Testable
	prop_equivalence x n	= Test.QuickCheck.label "prop_equivalence" $ Math.Implementations.Factorial.risingFactorial x n == sign * Math.Implementations.Factorial.fallingFactorial (negate x) n && Math.Implementations.Factorial.fallingFactorial x n == sign * Math.Implementations.Factorial.risingFactorial (negate x) n	where
		sign :: Integer
		sign
			| even n	= 1
			| otherwise	= negate 1

	prop_symmetry x n	= Test.QuickCheck.label "prop_symmetry" $ Math.Implementations.Factorial.risingFactorial x n == Math.Implementations.Factorial.fallingFactorial (pred $ x + n) n

	prop_x0 x _		= Test.QuickCheck.label "prop_x0" $ all (== 1) $ map ($ 0) [Math.Implementations.Factorial.risingFactorial x, Math.Implementations.Factorial.fallingFactorial x]

	prop_0n _ n		= Test.QuickCheck.label "prop_0n" $ all (== if n == 0 then 1 else 0) $ map ($ n) [Math.Implementations.Factorial.risingFactorial 0, Math.Implementations.Factorial.fallingFactorial 0]

	prop_ratio :: Math.Implementations.Factorial.Algorithm -> Integer -> Integer -> Test.QuickCheck.Property
	prop_ratio algorithm i j	= Test.QuickCheck.label "prop_ratio" $ n !/! d == Math.Factorial.factorial algorithm n % Math.Factorial.factorial algorithm d	where
		n	= pred $ i `mod` 100000
		d	= pred $ j `mod` 100000

	prop_consistency :: Math.Implementations.Factorial.Algorithm -> Math.Implementations.Factorial.Algorithm -> Integer -> Test.QuickCheck.Property
	prop_consistency l r i	= l /= r	==> Test.QuickCheck.label "prop_consistency" $ Math.Factorial.factorial l n == Math.Factorial.factorial r n	where
		n	= pred $ i `mod` 100000

