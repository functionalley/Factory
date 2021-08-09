{-# LANGUAGE CPP #-}
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

 [@DESCRIPTION@]	Defines /QuickCheck/-properties for "Math.Pi".
-}

module Factory.Test.QuickCheck.Pi(
-- * Constants
	results,
-- * Types
-- ** Type-synonyms
--	Testable
) where

import			Factory.Test.QuickCheck.Factorial()
import			Factory.Test.QuickCheck.SquareRoot()
import qualified	Factory.Math.Implementations.Factorial			as Math.Implementations.Factorial
import qualified	Factory.Math.Implementations.Pi.AGM.Algorithm		as Math.Implementations.Pi.AGM.Algorithm
import qualified	Factory.Math.Implementations.Pi.BBP.Algorithm		as Math.Implementations.Pi.BBP.Algorithm
import qualified	Factory.Math.Implementations.Pi.Borwein.Algorithm	as Math.Implementations.Pi.Borwein.Algorithm
import qualified	Factory.Math.Implementations.Pi.Ramanujan.Algorithm	as Math.Implementations.Pi.Ramanujan.Algorithm
import qualified	Factory.Math.Implementations.Pi.Spigot.Algorithm	as Math.Implementations.Pi.Spigot.Algorithm
import qualified	Factory.Math.Implementations.SquareRoot			as Math.Implementations.SquareRoot
import qualified	Factory.Math.Pi						as Math.Pi
import qualified	Factory.Math.Precision					as Math.Precision
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))

#if !MIN_VERSION_base(4,8,0)
import	Control.Applicative((<$>), (<*>))
#endif

instance Test.QuickCheck.Arbitrary squareRootAlgorithm => Test.QuickCheck.Arbitrary (Math.Implementations.Pi.AGM.Algorithm.Algorithm squareRootAlgorithm)	where
	arbitrary	= Math.Implementations.Pi.AGM.Algorithm.BrentSalamin <$> Test.QuickCheck.arbitrary

instance Test.QuickCheck.Arbitrary Math.Implementations.Pi.BBP.Algorithm.Algorithm	where
	arbitrary	= Test.QuickCheck.elements [Math.Implementations.Pi.BBP.Algorithm.Bellard, Math.Implementations.Pi.BBP.Algorithm.Base65536]

instance (
	Test.QuickCheck.Arbitrary	squareRootAlgorithm,
	Test.QuickCheck.Arbitrary	factorialAlgorithm
 ) => Test.QuickCheck.Arbitrary (Math.Implementations.Pi.Borwein.Algorithm.Algorithm squareRootAlgorithm factorialAlgorithm)	where
	arbitrary	= Test.QuickCheck.oneof [
		Math.Implementations.Pi.Borwein.Algorithm.Borwein1993 <$> Test.QuickCheck.arbitrary <*> Test.QuickCheck.arbitrary
	 ]

instance (
	Test.QuickCheck.Arbitrary	squareRootAlgorithm,
	Test.QuickCheck.Arbitrary	factorialAlgorithm
 ) => Test.QuickCheck.Arbitrary (Math.Implementations.Pi.Ramanujan.Algorithm.Algorithm squareRootAlgorithm factorialAlgorithm)	where
	arbitrary	= Test.QuickCheck.oneof [
		Math.Implementations.Pi.Ramanujan.Algorithm.Classic <$> Test.QuickCheck.arbitrary <*> Test.QuickCheck.arbitrary,
		Math.Implementations.Pi.Ramanujan.Algorithm.Chudnovsky <$> Test.QuickCheck.arbitrary <*> Test.QuickCheck.arbitrary
	 ]

instance Test.QuickCheck.Arbitrary Math.Implementations.Pi.Spigot.Algorithm.Algorithm	where
	arbitrary	= Test.QuickCheck.elements [Math.Implementations.Pi.Spigot.Algorithm.RabinowitzWagon, Math.Implementations.Pi.Spigot.Algorithm.Gosper]

instance (
	Test.QuickCheck.Arbitrary agm,
	Test.QuickCheck.Arbitrary bbp,
	Test.QuickCheck.Arbitrary borwein,
	Test.QuickCheck.Arbitrary ramanujan,
	Test.QuickCheck.Arbitrary spigot
 ) => Test.QuickCheck.Arbitrary (Math.Pi.Category agm bbp borwein ramanujan spigot)	where
	arbitrary	= Test.QuickCheck.oneof [
		Math.Pi.AGM <$> Test.QuickCheck.arbitrary,
		Math.Pi.BBP <$> Test.QuickCheck.arbitrary,
		Math.Pi.Borwein <$> Test.QuickCheck.arbitrary,
		Math.Pi.Ramanujan <$> Test.QuickCheck.arbitrary,
		Math.Pi.Spigot <$> Test.QuickCheck.arbitrary
	 ]

type Category	= Math.Pi.Category (
	Math.Implementations.Pi.AGM.Algorithm.Algorithm Math.Implementations.SquareRoot.Algorithm
 ) Math.Implementations.Pi.BBP.Algorithm.Algorithm (
	Math.Implementations.Pi.Borwein.Algorithm.Algorithm Math.Implementations.SquareRoot.Algorithm Math.Implementations.Factorial.Algorithm
 ) (
	Math.Implementations.Pi.Ramanujan.Algorithm.Algorithm Math.Implementations.SquareRoot.Algorithm Math.Implementations.Factorial.Algorithm
 ) Math.Implementations.Pi.Spigot.Algorithm.Algorithm

type Testable	= Category -> Category -> Math.Precision.DecimalDigits -> Test.QuickCheck.Property

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= mapM (
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs { Test.QuickCheck.maxSuccess = 256 }
 ) [prop_consistency]	where
	prop_consistency :: Testable
	prop_consistency l r decimalDigits	= l /= r	==> Test.QuickCheck.label "prop_consistency" $ Math.Pi.openI l decimalDigits' - Math.Pi.openI r decimalDigits' <= 1 {-rounding error-}	where
		decimalDigits'	= succ $ decimalDigits `mod` 250

