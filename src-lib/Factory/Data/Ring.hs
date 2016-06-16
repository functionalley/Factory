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

	* Describes a /ring/ and operations on its members.

	* <https://en.wikipedia.org/wiki/Ring_%28mathematics%29>.

	* <http://www.numericana.com/answer/rings.htm>.
-}

module Factory.Data.Ring(
-- * Type-classes
	Ring(..),
-- * Types
-- ** Data.types
--	Product,
--	Sum,
-- * Functions
	product',
	sum',
-- ** Operators
	(=^)
) where

import qualified	Data.Monoid
import qualified	Factory.Math.DivideAndConquer	as Math.DivideAndConquer

infixl 6 =+=	-- Same as (+).
infixl 6 =-=	-- Same as (-).
infixl 7 =*=	-- Same as (*).
infixr 8 =^	-- Same as (^).

{- |
	* Define both the operations applicable to all members of the /ring/, and its mandatory members.

	* Minimal definition; '=+=', '=*=', 'additiveInverse', 'multiplicativeIdentity', 'additiveIdentity'.
-}
class Ring r	where
	(=+=)			:: r -> r -> r	-- ^ Addition of two members; required to be /commutative/; <https://en.wikipedia.org/wiki/Commutativity>.
	(=*=)			:: r -> r -> r	-- ^ Multiplication of two members.
	additiveInverse		:: r -> r	-- ^ The operand required to yield /zero/ under addition; <https://en.wikipedia.org/wiki/Additive_inverse>.
	multiplicativeIdentity	:: r		-- ^ The /identity/-member under multiplication; <http://mathworld.wolfram.com/MultiplicativeIdentity.html>.
	additiveIdentity	:: r		-- ^ The /identity/-member under addition (AKA /zero/); <https://en.wikipedia.org/wiki/Additive_identity>.

	(=-=) :: r -> r -> r			-- ^ Subtract the two specified /ring/-members.
	l =-= r	= l =+= additiveInverse r	-- Default implementation.

	square :: r -> r			-- ^ Square the ring.
	square r	= r =*= r		-- Default implementation; there may be a more efficient one.

{- |
	* Raise a /ring/-member to the specified positive integral power.

	* Exponentiation is implemented as a sequence of either squares of, or multiplications by, the /ring/-member;
	<https://en.wikipedia.org/wiki/Exponentiation_by_squaring>.
-}
(=^) :: (
	Eq		r,
	Integral	power,
	Ring		r,
	Show		power
 ) => r -> power -> r
_ =^ 0	= multiplicativeIdentity
ring =^ power
	| power < 0							= error $ "Factory.Data.Ring.(=^):\tthe result isn't guaranteed to be a ring-member, for power=" ++ show power
	| ring `elem` [additiveIdentity, multiplicativeIdentity]	= ring
	| otherwise							= slave power
	where
		slave 1	= ring
		slave n	= (if r == 0 {-even-} then id else (=*= ring)) . square $ slave q	where
			(q, r)	= n `quotRem` 2

-- | Does for 'Ring', what 'Data.Monoid.Product' does for type 'Num', in that it makes it an instance of 'Data.Monoid.Monoid' under multiplication.
newtype Product p	= MkProduct {
	getProduct :: p	-- ^ Access the polymorphic payload.
} deriving (Read, Show)

instance Ring r => Data.Monoid.Monoid (Product r)	where
	mempty					= MkProduct multiplicativeIdentity
	MkProduct x `mappend` MkProduct y	= MkProduct $ x =*= y

-- | Returns the /product/ of the list of /ring/-members.
product' :: Ring r => Math.DivideAndConquer.BisectionRatio -> Math.DivideAndConquer.MinLength -> [r] -> r
-- product' _ _			= getProduct . Data.Monoid.mconcat . map MkProduct
product' ratio minLength	= getProduct . Math.DivideAndConquer.divideAndConquer ratio minLength . map MkProduct

-- | Does for 'Ring', what 'Data.Monoid.Sum' does for type 'Num', in that it makes it an instance of 'Data.Monoid.Monoid' under addition.
newtype Sum s	= MkSum {
	getSum :: s	-- ^ Access the polymorphic payload.
} deriving (Read, Show)

instance Ring r => Data.Monoid.Monoid (Sum r)	where
	mempty				= MkSum additiveIdentity
	MkSum x `mappend` MkSum y	= MkSum $ x =+= y

-- | Returns the /sum/ of the list of /ring/-members.
sum' :: Ring r => Math.DivideAndConquer.BisectionRatio -> Math.DivideAndConquer.MinLength -> [r] -> r
-- sum' _ _		= getSum . Data.Monoid.mconcat . map MkSum
sum' ratio minLength	= getSum . Math.DivideAndConquer.divideAndConquer ratio minLength . map MkSum

