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

 [@DESCRIPTION@]	Defines /Borwein/ series for /Pi/; <http://en.wikipedia.org/wiki/Borwein%27s_algorithm>
-}

module Factory.Math.Implementations.Pi.Borwein.Implementation(
-- * Functions
	openR
) where

import qualified	Control.Arrow
import qualified	Control.Parallel.Strategies
import qualified	Factory.Math.Implementations.Pi.Borwein.Series	as Math.Implementations.Pi.Borwein.Series
import qualified	Factory.Math.Precision				as Math.Precision

-- | Returns /Pi/, accurate to the specified number of decimal digits.
openR
	:: Math.Implementations.Pi.Borwein.Series.Series squareRootAlgorithm factorialAlgorithm	-- ^ This /Pi/-algorithm is parameterised by the type of other algorithms to use.
	-> squareRootAlgorithm									-- ^ The specific /square-root/ algorithm to apply to the above series.
	-> factorialAlgorithm									-- ^ The specific /factorial/-algorithm to apply to the above series.
	-> Math.Precision.DecimalDigits								-- ^ The number of decimal digits required.
	-> Rational
openR Math.Implementations.Pi.Borwein.Series.MkSeries {
	Math.Implementations.Pi.Borwein.Series.terms		= terms,
	Math.Implementations.Pi.Borwein.Series.convergenceRate	= convergenceRate
} squareRootAlgorithm factorialAlgorithm decimalDigits	= uncurry (/) . Control.Parallel.Strategies.withStrategy (
		Control.Parallel.Strategies.parTuple2 Control.Parallel.Strategies.rdeepseq Control.Parallel.Strategies.rdeepseq
	) . Control.Arrow.second (
		sum . take (
			Math.Precision.getTermsRequired convergenceRate decimalDigits
		)
	) $ terms squareRootAlgorithm factorialAlgorithm decimalDigits

