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

 [@DESCRIPTION@]	Exports functions involving integral powers.
-}

module Factory.Math.Power(
-- * Functions
	square,
	squaresFrom,
	cube,
	cubeRoot,
	raiseModulo
) where

-- | Mainly for convenience.
square :: Num n => n -> n
square x	= x ^ (2 :: Int)	-- CAVEAT: this could be eta-reduced, but it won't then inline when called with a single argument.

{-# INLINE square #-}

-- | Just for convenience.
cube :: Num n => n -> n
cube	= (^ (3 :: Int))

{- |
	* Iteratively generate sequential /squares/, from the specified initial value,
	based on the fact that @(x + 1)^2 = x^2 + 2 * x + 1@.

	* The initial value doesn't need to be either positive or integral.
-}
squaresFrom :: (Enum n, Num n)
	=> n		-- ^ Lower bound.
	-> [(n, n)]	-- ^ @ [(n, n^2)] @.
squaresFrom from	= iterate (\(x, y) -> (succ x, succ $ y + 2 * x)) (from, square from)

-- | Just for convenience.
cubeRoot :: Double -> Double
cubeRoot	= (** recip 3)

{- |
	* Raise an arbitrary number to the specified positive integral power, using /modular/ arithmetic.

	* Implements exponentiation as a sequence of either /squares/ or multiplications by the base;
	<https://en.wikipedia.org/wiki/Exponentiation_by_squaring>.

	* <https://en.wikipedia.org/wiki/Modular_exponentiation>.
-}
raiseModulo :: (Integral i, Integral power, Show power)
	=> i	-- ^ Base.
	-> power
	-> i	-- ^ Modulus.
	-> i	-- ^ Result.
raiseModulo _ _ 0	= error "Factory.Math.Power.raiseModulo:\tzero modulus."
raiseModulo _ _ 1	= 0
raiseModulo _ 0 modulus	= 1 `mod` modulus
raiseModulo base power modulus
	| base < 0		= (`mod` modulus) . (if even power then id else negate) $ raiseModulo (negate base) power modulus	-- Recurse.
	| power < 0		= error $ "Factory.Math.Power.raiseModulo:\tnegative power; " ++ show power
	| first `elem` [0, 1]	= first
	| otherwise		= slave power
	where
		first	= base `mod` modulus

		slave 1	= first
		slave e	= (`mod` modulus) . (if r == 0 {-even-} then id else (* base)) . square $ slave q {-recurse-}	where
			(q, r)	= e `quotRem` 2

