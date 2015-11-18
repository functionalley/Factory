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

	* Provides a polymorphic algorithm, to /unfold/ a list into a tree, to which an /associative binary operator/ is then applied to re-/fold/ the tree to a /scalar/.

	* Implementations of this strategy have been provided for /addition/ and /multiplication/,
	though other associative binary operators, like 'gcd' or 'lcm' could also be used.

	* Where the contents of the list are consecutive, a more efficient implementation is available in /Factory.Data.Interval/.
-}

module Factory.Math.DivideAndConquer(
-- * Types
-- ** Type-synonyms
	BisectionRatio,
	MinLength,
-- * Functions
	divideAndConquer,
	product',
	sum'
) where

import			Control.Arrow((***))
import qualified	Control.Parallel.Strategies
import qualified	Data.Monoid
import qualified	Data.Ratio

{- |
	* The ratio of the original list-length at which to bisect.

	* CAVEAT: the value can overflow.
-}
type BisectionRatio	= Data.Ratio.Ratio Int

-- | The list-length beneath which to terminate bisection.
type MinLength	= Int

{- |
	* Reduces a list to a single scalar encapsulated in a 'Data.Monoid.Monoid',
	using a /divide-and-conquer/ strategy,
	bisecting the list and recursively evaluating each part; <http://en.wikipedia.org/wiki/Divide_and_conquer_algorithm>.

	* By choosing a 'bisectionRatio' other than @(1 % 2)@, the bisection can be made asymmetrical.
	The specified ratio represents the length of the left-hand portion, over the original list-length;
	eg. @(1 % 3)@ results in the first part, half the length of the second.

	* This process of recursive bisection, is terminated beneath the specified minimum list-length,
	after which the /monoid/'s binary operator is directly /folded/ over the list.

	* One can view this as a <http://en.wikipedia.org/wiki/Hylomorphism_%28computer_science%29>,
	in which the list is exploded into a binary tree-structure
	(each leaf of which contains a list of up to 'minLength' integers, and each node of which contains an associative binary operator),
	and then collapsed to a scalar, by application of the operators.
-}
divideAndConquer :: Data.Monoid.Monoid monoid
	=> BisectionRatio	-- ^ The ratio of the original list-length at which to bisect.
	-> MinLength		-- ^ For efficiency, the list will not be bisected, when it's length has been reduced to this value.
	-> [monoid]		-- ^ The list on which to operate.
	-> monoid		-- ^ The resulting scalar.
divideAndConquer bisectionRatio minLength l
	| any ($ apportion minLength) [
		(< 1),			-- The left-hand list may be null.
		(> pred minLength)	-- The right-hand list may be null.
	]		= error $ "Factory.Math.DivideAndConquer.divideAndConquer:\tbisectionRatio='" ++ show bisectionRatio ++ "' is incompatible with minLength=" ++ show minLength ++ "."
	| otherwise	= slave (length l) l
	where
		apportion :: Int -> Int
		apportion list	= (list * Data.Ratio.numerator bisectionRatio) `div` Data.Ratio.denominator bisectionRatio

		slave len list
			| len <= minLength	= Data.Monoid.mconcat list	-- Fold the monoid's binary operator over the list.
			| otherwise		= uncurry Data.Monoid.mappend . Control.Parallel.Strategies.withStrategy (
				Control.Parallel.Strategies.parTuple2 Control.Parallel.Strategies.rseq Control.Parallel.Strategies.rseq
			) . (slave cut *** slave (len - cut)) $ splitAt cut list	where	-- Apply the monoid's binary operator to the two operands resulting from bisection.
				cut	= apportion len

{- |
	* Multiplies the specified list of numbers.

	* Since the result can be large, 'divideAndConquer' is used in an attempt to form operands of a similar order of magnitude,
	which creates scope for the use of more efficient multiplication-algorithms.
-}
product' :: Num n
	=> BisectionRatio	-- ^ The ratio of the original list-length at which to bisect.
	-> MinLength		-- ^ For efficiency, the list will not be bisected, when it's length has been reduced to this value.
	-> [n]			-- ^ The numbers whose product is required.
	-> n			-- ^ The resulting product.
product' bisectionRatio minLength	= Data.Monoid.getProduct . divideAndConquer bisectionRatio minLength . map Data.Monoid.Product

{- |
	* Sums the specified list of numbers.

	* Since the result can be large, 'divideAndConquer' is used in an attempt to form operands of a similar order of magnitude,
	which creates scope for the use of more efficient multiplication-algorithms.
	/Multiplication/ is required for the /addition/ of 'Rational' numbers by cross-multiplication;
	this function is unlikely to be useful for other numbers.
-}
sum' :: Num n
	=> BisectionRatio	-- ^ The ratio of the original list-length at which to bisect.
	-> MinLength		-- ^ For efficiency, the list will not be bisected, when it's length has been reduced to this value.
	-> [n]			-- ^ The numbers whose sum is required.
	-> n			-- ^ The resulting sum.
sum' bisectionRatio minLength	= Data.Monoid.getSum . divideAndConquer bisectionRatio minLength . map Data.Monoid.Sum

