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

 [@DESCRIPTION@]	Implements a /Ramanujan/-type series for /Pi/; <http://en.wikipedia.org/wiki/Srinivasa_Ramanujan>.
-}

module Factory.Math.Implementations.Pi.Ramanujan.Implementation(
-- * Functions
	openR
) where

import qualified	Control.Parallel.Strategies
import qualified	Factory.Math.Implementations.Pi.Ramanujan.Series	as Math.Implementations.Pi.Ramanujan.Series
import qualified	Factory.Math.Precision					as Math.Precision
import qualified	Factory.Math.Summation					as Math.Summation

-- | Returns /Pi/, accurate to the specified number of decimal digits.
openR
	:: Math.Implementations.Pi.Ramanujan.Series.Series squareRootAlgorithm factorialAlgorithm	-- ^ This /Pi/-algorithm is parameterised by the type of other algorithms to use.
	-> squareRootAlgorithm										-- ^ The specific /square-root/ algorithm to apply to the above series.
	-> factorialAlgorithm										-- ^ The specific /factorial/-algorithm to apply to the above series.
	-> Math.Precision.DecimalDigits									-- ^ The number of decimal digits required.
	-> Rational
openR Math.Implementations.Pi.Ramanujan.Series.MkSeries {
	Math.Implementations.Pi.Ramanujan.Series.terms			= terms,
	Math.Implementations.Pi.Ramanujan.Series.getSeriesScalingFactor	= getSeriesScalingFactor,
	Math.Implementations.Pi.Ramanujan.Series.convergenceRate	= convergenceRate
} squareRootAlgorithm factorialAlgorithm decimalDigits	= uncurry (/) $ Control.Parallel.Strategies.withStrategy (
		Control.Parallel.Strategies.parTuple2 Control.Parallel.Strategies.rdeepseq Control.Parallel.Strategies.rdeepseq
	) (
		getSeriesScalingFactor squareRootAlgorithm decimalDigits,
		Math.Summation.sumR 64 . take (
			Math.Precision.getTermsRequired convergenceRate decimalDigits
		) $ terms factorialAlgorithm
	) -- Pair.

