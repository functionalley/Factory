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

	* Describes a list of /prime factors/.

	* The product of this list of prime-factors represents the /composite/ integer from which they were originally extracted.
-}

module Factory.Data.PrimeFactors(
-- * Types
-- ** Type-synonyms
	Factors,
-- * Functions
	insert',
--	invert,
	product',
	reduce,
--	reduceSorted,
--	sumExponents,
-- ** Operators
	(>*<),
	(>/<),
	(>^)
) where

import qualified	Control.Arrow
import			Control.Arrow((&&&))
import qualified	Data.List
import qualified	Data.Ord
import qualified	Factory.Math.DivideAndConquer	as Math.DivideAndConquer
import qualified	Factory.Data.Exponential	as Data.Exponential
import			Factory.Data.Exponential((<^), (=~))
import qualified	ToolShed.Data.List

infixl 7 >/<, >*<	-- Same as (/).
infixr 8 >^		-- Same as (^).

{- |
	* Each element of this list represents one /prime-factor/, expressed as an /exponential/ with a /prime/ base, of the original integer.

	* Whilst it only makes sense for both the /base/ and /exponent/ to be integral, these constrains are applied at the function-level as required.
-}
type Factors base exponent	= [Data.Exponential.Exponential base exponent]

{- |
	* Sorts a list representing a product of /prime factors/ by increasing /base/.

	* Multiplies 'Data.Exponential.Exponential's of similar /base/.
-}
reduce :: (Ord base, Num exponent, Ord exponent) => Factors base exponent -> Factors base exponent
reduce	= reduceSorted . Data.List.sort {-primarily by base-}

-- | Multiplies 'Data.Exponential.Exponential's of similar /base/.
reduceSorted :: (Eq base, Num exponent) => Factors base exponent -> Factors base exponent
-- reduceSorted	= map (Data.Exponential.getBase . head &&& sumExponents) . Data.List.groupBy (=~)	-- Slow
reduceSorted []	= []
reduceSorted (x : xs)
	| null matched	= x : reduceSorted remainder
	| otherwise	= Control.Arrow.second (+ sumExponents matched) x : reduceSorted remainder
	where
		(matched, remainder)	= span (=~ x) xs

{- |
	* Insert a 'Data.Exponential.Exponential', into a list representing a product of /prime factors/, multiplying with any incumbent of like /base/.

	* The list should be sorted by increasing /base/.

	* Preserves the sort-order.

	* CAVEAT: this is tolerably efficient for sporadic insertion; to insert a list, use '>*<'.
-}
insert' :: (Ord base, Num exponent) => Data.Exponential.Exponential base exponent -> Factors base exponent -> Factors base exponent
insert' e []		= [e]
insert' e l@(x : xs)	= case Data.Ord.comparing Data.Exponential.getBase e x of
	LT	-> e : l
	GT	-> x : insert' e xs	-- Recurse.
	_	-> Control.Arrow.second (+ Data.Exponential.getExponent e) x : xs	-- Multiply by adding exponents.

{- |
	* Multiplies two lists each representing a product of /prime factors/, and sorted by increasing /base/.

	* Preserves the sort-order.
-}
(>*<) :: (Ord base, Num exponent, Ord exponent) => Factors base exponent -> Factors base exponent -> Factors base exponent
l >*< r	= reduceSorted $ ToolShed.Data.List.merge l r

-- | Invert the product of a list /prime factors/, by negating each of the /exponents/.
invert :: Num exponent => Factors base exponent -> Factors base exponent
invert	= map Data.Exponential.invert

{- |
	* Divides two lists, each representing a product of /prime factors/, and sorted by increasing /base/.

	* Preserves the sort-order.
-}
(>/<) :: (Integral base, Integral exponent)
	=> Factors base exponent				-- ^ The list of /prime factors/ in the /numerator/.
	-> Factors base exponent				-- ^ The list of /prime factors/ in the /denominator/.
	-> (Factors base exponent, Factors base exponent)	-- ^ The ratio of /numerator/ and /denominator/, after like /prime factors/ are cancelled.
numerator >/< denominator	= filter (
	(> 0) . Data.Exponential.getExponent
 ) &&& invert . filter (
	(< 0) . Data.Exponential.getExponent
 ) $ numerator >*< invert denominator

{- |
	* Raise the product of a list /prime factors/ to the specified power.

	* CAVEAT: this merely involves raising each element to the specified power; cf. raising a /polynomial/ to a power.
-}
(>^) :: Num exponent => Factors base exponent -> exponent -> Factors base exponent
factors >^ power	= map (<^ power) factors

-- | Sum the /exponents/ of the specified list; as required to multiply exponentials with identical /base/.
sumExponents :: Num exponent => Factors base exponent -> exponent
sumExponents	= foldr ((+) . Data.Exponential.getExponent) 0

-- | Multiply a list of /prime factors/.
product' :: (Num base, Integral exponent)
	=> Math.DivideAndConquer.BisectionRatio
	-> Math.DivideAndConquer.MinLength
	-> Factors base exponent		-- ^ The list on which to operate.
	-> base					-- ^ The result.
product' bisectionRatio minLength	= Math.DivideAndConquer.product' bisectionRatio minLength . map Data.Exponential.evaluate

