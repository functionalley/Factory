{-
	Copyright (C) 2011 Dr. Alistair Ward

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

 [@DESCRIPTION@]	Determines the /Arithmetic-geometric mean/; <https://en.wikipedia.org/wiki/Arithmetic-geometric_mean>.
-}

module Factory.Math.ArithmeticGeometricMean(
-- * Types
-- ** Type-synonyms
	ArithmeticMean,
	GeometricMean,
	AGM,
-- * Functions
	convergeToAGM,
	spread,
-- ** Accessors
	getArithmeticMean,
	getGeometricMean,
-- ** Predicates
	isValid
) where

import			Control.Arrow((&&&))
import qualified	Control.Parallel.Strategies
import qualified	Factory.Math.Precision	as Math.Precision
import qualified	Factory.Math.SquareRoot	as Math.SquareRoot

-- | The type of the /arithmetic mean/; <https://en.wikipedia.org/wiki/Arithmetic_mean>.
type ArithmeticMean	= Rational

-- | The type of the /geometric mean/; <https://en.wikipedia.org/wiki/Geometric_mean>.
type GeometricMean	= Rational

-- | Encapsulates both /arithmetic/ and /geometric/ means.
type AGM	= (ArithmeticMean, GeometricMean)

-- | Accessor.
{-# INLINE getArithmeticMean #-}
getArithmeticMean :: AGM -> ArithmeticMean
getArithmeticMean	= fst

-- | Accessor.
{-# INLINE getGeometricMean #-}
getGeometricMean :: AGM -> GeometricMean
getGeometricMean	= snd

-- | Returns an infinite list which converges on the /Arithmetic-geometric mean/.
convergeToAGM :: Math.SquareRoot.Algorithmic squareRootAlgorithm => squareRootAlgorithm -> Math.Precision.DecimalDigits -> AGM -> [AGM]
convergeToAGM squareRootAlgorithm decimalDigits agm
	| decimalDigits <= 0	= error $ "Factory.Math.ArithmeticGeometricMean.convergeToAGM:\tinvalid number of decimal digits; " ++ show decimalDigits
	| not $ isValid agm	= error $ "Factory.Math.ArithmeticGeometricMean.convergeToAGM:\tboth means must be positive for a real geometric mean; " ++ show agm
	| spread agm == 0	= repeat agm
	| otherwise		= let
		simplify :: Rational -> Rational
		simplify	= Math.Precision.simplify (pred decimalDigits {-ignore single integral digit-})	-- This makes a gigantic difference to performance.

		findArithmeticMean :: AGM -> ArithmeticMean
		findArithmeticMean	= (/ 2) . uncurry (+)

		findGeometricMean :: AGM -> GeometricMean
		findGeometricMean	= Math.SquareRoot.squareRoot squareRootAlgorithm decimalDigits . uncurry (*)
	in iterate (
		Control.Parallel.Strategies.withStrategy (
			Control.Parallel.Strategies.parTuple2 Control.Parallel.Strategies.rdeepseq Control.Parallel.Strategies.rdeepseq
		) . (simplify . findArithmeticMean &&& simplify . findGeometricMean)
	) agm

-- | Returns the bounds within which the 'AGM' has been constrained.
spread :: AGM -> Rational
spread	= uncurry (-)

-- | Checks that both /means/ are positive, as required for the /geometric mean/ to be consistently /real/.
isValid :: AGM -> Bool
isValid (a, g)	= all (>= 0) [a, g]

