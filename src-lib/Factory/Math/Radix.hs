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
	along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}
{- |
 [@AUTHOR@]	Dr. Alistair Ward

 [@DESCRIPTION@]	Facilitates representation of 'Integral' values in alternative 'Integral' bases.
-}

module Factory.Math.Radix(
-- * Constants
--	decodes,
--	digits,
--	encodes,
-- * Functions
	digitSum,
	digitalRoot,
	fromBase,
	toBase
) where

import			Data.Array.IArray((!))
import qualified	Data.Array.IArray
import qualified	Data.Char
import qualified	Data.List
import qualified	Data.Maybe

-- | Characters used to represent the digits of numbers in @(-36 <= base <= 36)@.
digits :: String
digits	= ['0' .. '9'] ++ ['a' .. 'z']

-- | Constant random-access lookup for 'digits'.
encodes :: (Data.Array.IArray.Ix index, Integral index) => Data.Array.IArray.Array index Char
encodes	= Data.Array.IArray.listArray (0, fromIntegral . pred $ length digits) digits

-- | Constant reverse-lookup for 'digits'.
decodes :: Integral i => [(Char, i)]
decodes	= zip digits [0 ..]

{- |
	* Convert the specified integral quantity, to an alternative base, and represent the result as a 'String'.

	* Both negative integers and negative bases are permissible.

	* The conversion to 'Char' can only succeed where printable and intelligible characters exist to represent all digits in the chosen base;
	which in practice means @(-36 <= base <= 36)@.
-}
toBase :: (
	Data.Array.IArray.Ix	decimal,
	Integral		base,
	Integral		decimal,
	Show			base,
	Show			decimal
 ) => base -> decimal -> String
toBase 10 decimal	= show decimal	-- Base unchanged.
toBase _ 0		= "0"		-- Zero has the same representation in any base.
toBase base decimal
	| abs base < 2			= error $ "Factory.Math.Radix.toBase:\tan arbitrary integer can't be represented in base " ++ show base
	| abs base > fromIntegral (
		length digits
	)				= error $ "Factory.Math.Radix.toBase:\tunable to clearly represent the complete set of digits in base " ++ show base
	| base > 0 && decimal < 0	= '-' : map toDigit (fromDecimal (negate decimal) [])
	| otherwise			= toDigit `map` fromDecimal decimal []
	where
		fromDecimal 0		= id
		fromDecimal n
			| remainder < 0	= fromDecimal (succ quotient) . ((remainder - fromIntegral base) :)	-- This can only occur when base is negative; cf. 'divMod'.
			| otherwise	= fromDecimal quotient . (remainder :)
			where
				(quotient, remainder)	= n `quotRem` fromIntegral base

		toDigit :: (Data.Array.IArray.Ix i, Integral i, Show i) => i -> Char
		toDigit n
			| n >&< encodes	= encodes ! n
			| otherwise	= error $ "Factory.Math.Radix.toBase.toDigit:\tno suitable character-representation for integer " ++ show n
			where
				(>&<) :: (Data.Array.IArray.Ix i) => i -> Data.Array.IArray.Array i Char -> Bool
				index >&< array	= ($ index) `all` [(>= lower), (<= upper)]	where
					(lower, upper)	= Data.Array.IArray.bounds array

{- |
	* Convert the 'String'-representation of a number in the specified base, to an integer.

	* Both negative numbers and negative bases are permissible.
-}
fromBase :: (
	Integral	base,
	Integral	decimal,
	Read		decimal,
	Show		base
 ) => base -> String -> decimal
fromBase 10 s	= read s	-- Base unchanged.
fromBase _ ""	= error "Factory.Math.Radix.fromBase:\tnull string."
fromBase _ "0"	= 0		-- Zero has the same representation in any base.
fromBase base s
	| abs base < 2			= error $ "Factory.Math.Radix.fromBase:\tan arbitrary integer can't be represented in base " ++ show base
	| abs base > fromIntegral (
		length digits
	)				= error $ "Factory.Math.Radix.fromBase:\tunable to clearly represent the complete set of digits in base " ++ show base
	| base > 0
	, '-' : remainder <- s	= negate $ fromBase base remainder	-- Recurse.
	| otherwise			= Data.List.foldl' (\l -> ((l * fromIntegral base) +) . fromDigit) 0 s	where
		fromDigit :: Integral i => Char -> i
		fromDigit c	= case c `lookup` decodes of
			Just i
				| i >= abs (fromIntegral base)	-> error $ "Factory.Math.Radix.fromBase.fromDigit:\tillegal char " ++ show c ++ ", for base " ++ show base
				| otherwise			-> i
			_					-> error $ "Factory.Math.Radix.fromBase.fromDigit:\tunrecognised char " ++ show c

{- |
	* <https://mathworld.wolfram.com/DigitSum.html>.

	* <https://en.wikipedia.org/wiki/Digit_sum>.
-}
digitSum :: (
	Data.Array.IArray.Ix	decimal,
	Integral		base,
	Integral		decimal,
	Show			base,
	Show			decimal
 ) => base -> decimal -> decimal
digitSum 10	= fromIntegral . foldr ((+) . Data.Char.digitToInt) 0 . show
digitSum base	= sum . Data.Maybe.mapMaybe (`lookup` decodes) . toBase base

-- | <https://en.wikipedia.org/wiki/Digital_root>.
digitalRoot :: (
	Data.Array.IArray.Ix	decimal,
	Integral		decimal,
	Show			decimal
 ) => decimal -> decimal
digitalRoot	= until (<= 9) (digitSum (10 :: Int))

