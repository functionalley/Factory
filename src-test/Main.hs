{-
	Copyright (C) 2015 Dr. Alistair Ward

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

 [@DESCRIPTION@]

	* The entry-point to the application's test-suite.
-}

module Main(main) where

import qualified	Control.Monad
import qualified	Factory.Test.QuickCheck.ArithmeticGeometricMean	as Test.QuickCheck.ArithmeticGeometricMean
import qualified	Factory.Test.QuickCheck.Factorial		as Test.QuickCheck.Factorial
import qualified	Factory.Test.QuickCheck.Hyperoperation		as Test.QuickCheck.Hyperoperation
import qualified	Factory.Test.QuickCheck.Interval		as Test.QuickCheck.Interval
import qualified	Factory.Test.QuickCheck.MonicPolynomial		as Test.QuickCheck.MonicPolynomial
import qualified	Factory.Test.QuickCheck.PerfectPower		as Test.QuickCheck.PerfectPower
import qualified	Factory.Test.QuickCheck.Pi			as Test.QuickCheck.Pi
import qualified	Factory.Test.QuickCheck.Polynomial		as Test.QuickCheck.Polynomial
import qualified	Factory.Test.QuickCheck.Power			as Test.QuickCheck.Power
import qualified	Factory.Test.QuickCheck.Primality		as Test.QuickCheck.Primality
import qualified	Factory.Test.QuickCheck.PrimeFactorisation	as Test.QuickCheck.PrimeFactorisation
import qualified	Factory.Test.QuickCheck.Primes			as Test.QuickCheck.Primes
import qualified	Factory.Test.QuickCheck.Probability		as Test.QuickCheck.Probability
import qualified	Factory.Test.QuickCheck.Radix			as Test.QuickCheck.Radix
import qualified	Factory.Test.QuickCheck.SquareRoot		as Test.QuickCheck.SquareRoot
import qualified	Factory.Test.QuickCheck.Statistics		as Test.QuickCheck.Statistics
import qualified	Factory.Test.QuickCheck.Summation		as Test.QuickCheck.Summation
import qualified	System.Exit
import qualified	ToolShed.Test.QuickCheck.Result

-- | Entry-point.
main :: IO ()
main	= mapM_ (
	(`Control.Monad.unless` System.Exit.exitFailure) . all ToolShed.Test.QuickCheck.Result.isSuccessful =<<
 ) [
	Test.QuickCheck.ArithmeticGeometricMean.results,
	Test.QuickCheck.Factorial.results,
	Test.QuickCheck.Hyperoperation.results,
	Test.QuickCheck.Interval.results,
	Test.QuickCheck.MonicPolynomial.results,
	Test.QuickCheck.PerfectPower.results,
	Test.QuickCheck.Pi.results,
	Test.QuickCheck.Polynomial.results,
	Test.QuickCheck.Power.results,
	Test.QuickCheck.Primality.results,
	Test.QuickCheck.PrimeFactorisation.results,
	Test.QuickCheck.Primes.results,
	Test.QuickCheck.Probability.results,
	Test.QuickCheck.Radix.results,
	Test.QuickCheck.SquareRoot.results,
	Test.QuickCheck.Statistics.results,
	Test.QuickCheck.Summation.results
 ]

