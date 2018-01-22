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
	along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}
{- |
 [@AUTHOR@]	Dr. Alistair Ward

 [@DESCRIPTION@]	Exports functions related to /perfect powers/.
-}

module Factory.Math.PerfectPower(
-- * Functions
	maybeSquareNumber,
-- ** Predicates
	isPerfectPower
--	isPerfectPowerInt
) where

import qualified	Data.IntSet
import qualified	Data.Set
import qualified	Factory.Math.Power	as Math.Power

{- |
	* Returns @(Just . sqrt)@ if the specified integer is a /square number/ (AKA /perfect square/).

	* <https://en.wikipedia.org/wiki/Square_number>.

	* <https://mathworld.wolfram.com/SquareNumber.html>.

	* @(Math.Power.square . sqrt)@ is expensive, so the modulus of the operand is tested first, in an attempt to prove it isn't a /perfect square/.
	The set of tests, and the valid moduli within each test, are ordered to maximize the rate of failure-detection.
-}
maybeSquareNumber :: Integral i => i -> Maybe i
maybeSquareNumber i
--	| i < 0					= Nothing	-- This function is performance-sensitive, but this test is neither strictly nor frequently required.
	| all (\(modulus, valid) -> rem i modulus `elem` valid) [
--							-- Distribution of moduli amongst perfect squares	Cumulative failure-detection.
		(16,	[0,1,4,9]),			-- All moduli are equally likely.			75%
		(9,	[0,1,4,7]),			-- Zero occurs 33%, the others only 22%.			88%
		(17,	[1,2,4,8,9,13,15,16,0]),	-- Zero only occurs 5.8%, the others 11.8%.		94%
-- These additional tests, aren't always cost-effective.
		(13,	[1,3,4,9,10,12,0]),		-- Zero only occurs 7.7%, the others 15.4%.		97%
		(7,	[1,2,4,0]),			-- Zero only occurs 14.3%, the others 28.6%.		98%
		(5,	[1,4,0])			-- Zero only occurs 20%, the others 40%.			99%

--	] && fromIntegral iSqrt == sqrt'	= Just iSqrt	-- CAVEAT: erroneously True for 187598574531033120 (187598574531033121 is square).
	] && Math.Power.square iSqrt == i	= Just iSqrt
	| otherwise				= Nothing
	where
		sqrt' :: Double
		sqrt'	= sqrt $ fromIntegral i

		iSqrt	= round sqrt'

{- |
	* An integer @(> 1)@ which can be expressed as an integral power @(> 1)@ of a smaller /natural/ number.

	* CAVEAT: /zero/ and /one/ are normally excluded from this set.

	* <https://en.wikipedia.org/wiki/Perfect_power>.

	* <https://mathworld.wolfram.com/PerfectPower.html>.

	* A generalisation of the concept of /perfect squares/, in which only the exponent '2' is significant.
-}
isPerfectPower :: Integral i => i -> Bool
isPerfectPower i
	| i < Math.Power.square 2	= False
	| otherwise			= i `Data.Set.member` foldr (
		\n set	-> if n `Data.Set.member` set
			then set
--			else Data.Set.union set . Data.Set.fromDistinctAscList . takeWhile (<= i) . iterate (* n) $ Math.Power.square n
			else foldr Data.Set.insert set . takeWhile (<= i) . iterate (* n) $ Math.Power.square n	-- Faster.
	) Data.Set.empty [2 .. round $ sqrt (fromIntegral i :: Double)]

{-# NOINLINE isPerfectPower #-}
{-# RULES "isPerfectPower/Int" isPerfectPower = isPerfectPowerInt #-}

-- | A specialisation of 'isPerfectPower'.
isPerfectPowerInt :: Int -> Bool
isPerfectPowerInt i
	| i < Math.Power.square 2	= False
	| otherwise			= i `Data.IntSet.member` foldr (
		\n set	-> if n `Data.IntSet.member` set
			then set
			else foldr Data.IntSet.insert set . takeWhile (<= i) . iterate (* n) $ Math.Power.square n
	) Data.IntSet.empty [2 .. round $ sqrt (fromIntegral i :: Double)]

