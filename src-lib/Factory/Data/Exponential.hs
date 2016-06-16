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

 [@DESCRIPTION@]

	* Describes a simple numeric type, designed to contain an /exponential/ number.

	* <https://en.wikipedia.org/wiki/Exponentiation>.
-}

module Factory.Data.Exponential(
-- * Types
-- ** Type-synonyms
	Exponential,
-- * Functions
	evaluate,
	invert,
-- ** Accessors
	getBase,
	getExponent,
-- ** Constructors
	rightIdentity,
-- ** Operators
	(<^),
	(=~)
) where

import qualified	Control.Arrow

infix 4 =~	-- Same as (==).
infixr 8 <^	-- Same as (^).

-- | Describes an /exponential/, in terms of its /base/ and /exponent/.
type Exponential base exponent	= (base, exponent)

-- | Accessor.
{-# INLINE getBase #-}
getBase :: Exponential base exponent -> base
getBase	= fst

-- | Accessor.
{-# INLINE getExponent #-}
getExponent :: Exponential base exponent -> exponent
getExponent	= snd

{- |
	* Construct an 'Exponential' merely raised to the 1st power.

	* The value of the resulting exponential is the same as specified 'base'; <https://en.wikipedia.org/wiki/Identity_element>.
-}
rightIdentity :: Num exponent => base -> Exponential base exponent
rightIdentity x	= (x, 1)

-- | Evaluate the specified 'Exponential', returning the resulting number.
{-# INLINE evaluate #-}
evaluate :: (Num base, Integral exponent) => Exponential base exponent -> base
evaluate	= uncurry (^)	-- CAVEAT: in this eta-reduced form, it'll only be inlined when called without arguments.

-- | True if the /bases/ are equal.
(=~) :: Eq base => Exponential base exponent -> Exponential base exponent -> Bool
(l, _) =~ (r, _)	= l == r

-- | Raise the specified 'Exponential' to a power.
(<^) :: Num exponent
	=> Exponential base exponent	-- ^ The operand.
	-> exponent			-- ^ The power to which the exponential is to be raised.
	-> Exponential base exponent	-- ^ The result.
(b, e) <^ power	= (b, e * power)

-- | Invert the value, by negating the exponent.
invert :: Num exponent => Exponential base exponent -> Exponential base exponent
invert	= Control.Arrow.second negate

