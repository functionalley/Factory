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

	* Describes a bounded set of, typically integral, quantities.

	* Operations have been defined, on the list of /consecutive/ quantities delimited by these endpoints.

	* The point is that if the list is composed from /consecutive/ quantities, the intermediate values can be inferred, rather than physically represented.

 [@CAVEATS@]

	* The API was driven top-down by its caller's requirements, rather than a bottom-up attempt to provide a complete interface.
	consequently there may be omissions from the view point of future callers.

	* Thought similar to the mathematical concept of an /interval/, the latter technically relates to /real/ numbers; <https://en.wikipedia.org/wiki/Interval_%28mathematics%29>.

	* No account has been made for /semi-closed/ or /open/ intervals.
-}

module Factory.Data.Interval(
-- * Types
-- ** Type-synonyms
	Interval,
-- * Constants
	closedUnitInterval,
	mkBounded,
-- * Functions
--	divideAndConquer,
	elem',
--	getLength,
	normalise,
	product',
	shift,
	splitAt',
	toList,
-- ** Accessors
	getMinBound,
	getMaxBound,
-- ** Constructors
	precisely,
-- ** Predicates
	isReversed
) where

import			Control.Arrow((***), (&&&))
import qualified	Control.Parallel.Strategies
import qualified	Data.Monoid
import qualified	Data.Ratio
import qualified	Data.Tuple
import qualified	ToolShed.Data.Pair

-- | Defines a closed (inclusive) interval of consecutive values.
type Interval endPoint	= (endPoint, endPoint)

-- | Accessor.
{-# INLINE getMinBound #-}
getMinBound :: Interval endPoint -> endPoint
getMinBound	= fst

-- | Accessor.
{-# INLINE getMaxBound #-}
getMaxBound :: Interval endPoint -> endPoint
getMaxBound	= snd

-- | Construct the /unsigned closed unit-interval/; <https://en.wikipedia.org/wiki/Unit_interval>.
closedUnitInterval :: Num n => Interval n
closedUnitInterval	= (0, 1)

-- | Construct an /interval/ from a bounded type.
mkBounded :: Bounded endPoint => Interval endPoint
mkBounded	= (minBound, maxBound)

-- | Construct an /interval/ from a single value.
precisely :: endPoint -> Interval endPoint
precisely	= id &&& id

-- | Shift of both /end-points/ of the /interval/ by the specified amount.
shift :: Num endPoint
	=> endPoint		-- ^ The magnitude of the require shift.
	-> Interval endPoint	-- ^ The interval to be shifted.
	-> Interval endPoint
shift i	= ToolShed.Data.Pair.mirror (+ i)

-- | True if the specified value is within the inclusive bounds of the /interval/.
elem' :: Ord endPoint => endPoint -> Interval endPoint -> Bool
elem' x	= uncurry (&&) . ((<= x) *** (x <=))

-- | True if 'getMinBound' exceeds 'getMaxBound' extent.
isReversed :: Ord endPoint => Interval endPoint -> Bool
isReversed	= uncurry (>)

-- | Swap the /end-points/ where they were originally reversed, but otherwise do nothing.
normalise :: Ord endPoint => Interval endPoint -> Interval endPoint
normalise b
	| isReversed b	= Data.Tuple.swap b
	| otherwise	= b

-- | Bisect the /interval/ at the specified /end-point/; which should be between the two existing /end-points/.
splitAt' :: (
	Enum	endPoint,
	Num	endPoint,
	Ord	endPoint,
	Show	endPoint
 ) => endPoint -> Interval endPoint -> (Interval endPoint, Interval endPoint)
splitAt' i interval@(l, r)
	| any ($ i) [(< l), (>= r)]	= error $ "Factory.Data.Interval.splitAt':\tunsuitable index=" ++ show i ++ " for interval=" ++ show interval ++ "."
	| otherwise			= ((l, i), (succ i, r))

{- |
	* The distance between the endpoints,
	which for 'Integral' quantities is the same as the number of items in closed interval; though the latter concept would return type 'Int'.

	* CAVEAT: the implementation accounts for the potential fence-post error, for closed intervals of integers,
	but this results in the opposite error when used with /Fractional/ quantities.
	So, though most of the module merely requires 'Enum', this function is further restricted to 'Integral'.
-}
{-# INLINE getLength #-}
getLength :: Integral endPoint => Interval endPoint -> endPoint
getLength (l, r)	= succ r - l

{- |
	* Converts 'Interval' to a list by enumerating the values.

	* CAVEAT: produces rather odd results for 'Fractional' types, but no stranger than considering such types Enumerable in the first place.
-}
{-# INLINE toList #-}
toList :: Enum endPoint => Interval endPoint -> [endPoint]
toList	= uncurry enumFromTo	-- CAVEAT: in this eta-reduced form, it'll only be inlined when called without arguments.

{- |
	* Reduces 'Interval' to a single integral value encapsulated in a 'Data.Monoid.Monoid',
	using a /divide-and-conquer/ strategy,
	bisecting the /interval/ and recursively evaluating each part; <https://en.wikipedia.org/wiki/Divide_and_conquer_algorithm>.

	* By choosing a 'ratio' other than @(1 % 2)@, the bisection can be made asymmetrical.
	The specified ratio represents the length of the left-hand portion over the original list-length;
	eg. @(1 % 3)@ results in the first part, half the length of the second.

	* This process of recursive bisection, is terminated beneath the specified minimum length,
	after which the 'Interval' are expanded into the corresponding list, and the /monoid/'s binary operator is directly /folded/ over it.

	* One can view this as a <https://en.wikipedia.org/wiki/Hylomorphism_%28computer_science%29>,
	in which 'Interval' is exploded into a binary tree-structure
	(each leaf of which contains a list of up to 'minLength' integers, and each node of which contains an associative binary operator),
	and then collapsed to a scalar, by application of the operators.
-}
divideAndConquer :: (Data.Monoid.Monoid monoid, Integral i, Show i)
	=> (i -> monoid)	-- ^ The monoid's constructor.
	-> Data.Ratio.Ratio i	-- ^ The ratio of the original span, at which to bisect the 'Interval'.
	-> i			-- ^ For efficiency, the /interval/ will not be bisected, when it's length has been reduced to this value.
	-> Interval i
	-> monoid		-- ^ The resulting scalar.
divideAndConquer monoidConstructor ratio minLength
	| any ($ ratio) [
		(< 0),
		(>= 1)
	]		= error $ "Factory.Data.Interval.divideAndConquer:\tunsuitable ratio='" ++ show ratio ++ "'."
	| minLength < 1	= error $ "Factory.Data.Interval.divideAndConquer:\tunsuitable minLength=" ++ show minLength ++ "."
	| otherwise	= slave
	where
		slave interval@(l, r)
			| getLength interval <= minLength	= Data.Monoid.mconcat . map monoidConstructor $ toList interval	-- Fold the monoid's binary operator over the delimited list.
			| otherwise				= uncurry Data.Monoid.mappend . Control.Parallel.Strategies.withStrategy (
				Control.Parallel.Strategies.parTuple2 Control.Parallel.Strategies.rseq Control.Parallel.Strategies.rseq
			) . ToolShed.Data.Pair.mirror slave $ splitAt' (
				l + (r - l) * Data.Ratio.numerator ratio `div` Data.Ratio.denominator ratio	-- Use the ratio to generate the split-index.
			) interval	-- Apply the monoid's binary operator to the two operands resulting from bisection.

{- |
	* Multiplies the consecutive sequence of integers within 'Interval'.

	* Since the result can be large, 'divideAndConquer' is used to form operands of a similar order of magnitude,
	thus improving the efficiency of the big-number multiplication.
-}
product' :: (Integral i, Show i)
	=> Data.Ratio.Ratio i	-- ^ The ratio at which to bisect the 'Interval'.
	-> i			-- ^ For efficiency, the /interval/ will not be bisected, when it's length has been reduced to this value.
	-> Interval i
	-> i			-- ^ The resulting product.
product' ratio minLength interval
	| elem' 0 interval	= 0
	| otherwise		= Data.Monoid.getProduct $ divideAndConquer Data.Monoid.Product ratio minLength interval

