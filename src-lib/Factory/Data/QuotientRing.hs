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

	* Describes a /Quotient Ring/; <https://en.wikipedia.org/wiki/Quotient_ring>.

	* This is a /ring/ composed from a residue-class resulting from /modular/ division.
-}

module Factory.Data.QuotientRing(
-- * Type-classes
	QuotientRing(..),
-- * Functions
	quot',
	rem',
-- ** Predicates
	areCongruentModulo,
	isDivisibleBy
) where

import			Factory.Data.Ring((=-=))
import qualified	Factory.Data.Ring	as Data.Ring

-- | Defines a sub-class of 'Data.Ring.Ring', in which division is implemented.
class Data.Ring.Ring q => QuotientRing q	where
	quotRem'	:: q -> q -> (q, q)	-- ^ Divides the first operand by the second, to yield a pair composed from the /quotient/ and the /remainder/.

-- | Returns the /quotient/, after division of the two specified 'QuotientRing's.
quot' :: QuotientRing q
	=> q	-- ^ Numerator.
	-> q	-- ^ Denominator.
	-> q
quot' numerator	= fst . quotRem' numerator

-- | Returns the /remainder/, after division of the two specified 'QuotientRing's.
rem' :: QuotientRing q
	=> q	-- ^ Numerator.
	-> q	-- ^ Denominator.
	-> q
rem' numerator	= snd . quotRem' numerator

{- |
	* 'True' if the two specified 'QuotientRing's are /congruent/ in /modulo/-arithmetic, where the /modulus/ is a third 'QuotientRing'.

	* <http://www.usna.edu/Users/math/wdj/book/node74.html>.
-}
areCongruentModulo :: (Eq q, QuotientRing q)
	=> q	-- ^ LHS.
	-> q	-- ^ RHS.
	-> q	-- ^ Modulus.
	-> Bool
areCongruentModulo l r modulus
	| l == r	= True	-- Only required for efficiency.
	| otherwise	= (l =-= r) `isDivisibleBy` modulus

-- | True if the second operand /divides/ the first.
isDivisibleBy :: (Eq q, QuotientRing q)
	=> q	-- ^ Numerator.
	-> q	-- ^ Denominator.
	-> Bool
numerator `isDivisibleBy` denominator	= rem' numerator denominator == Data.Ring.additiveIdentity

