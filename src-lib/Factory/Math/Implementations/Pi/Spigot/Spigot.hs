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

	* Implements a /spigot/-algorithm; <https://en.wikipedia.org/wiki/Spigot_algorithm>.

	* Uses the traditional algorithm, rather than the /unbounded/ algorithm described by <http://www.comlab.ox.ac.uk/jeremy.gibbons/publications/spigot.pdf>.
-}

module Factory.Math.Implementations.Pi.Spigot.Spigot(
-- * Types
-- ** Type-synonyms
--	Base,
--	Coefficients,
--	I,
--	Pi,
--	PreDigits,
--	QuotRem,
-- * Constants
	decimal,
-- * Functions
--	carryAndDivide,
--	processColumns,
	openI,
-- ** Accessors
--	getQuotient,
--	getRemainder,
-- ** Constructors
--	mkRow
) where

import qualified	Control.Arrow
import qualified	Data.Char
import qualified	Data.Ratio
import qualified	Factory.Math.Implementations.Pi.Spigot.Series	as Math.Implementations.Pi.Spigot.Series
import qualified	Factory.Math.Precision				as Math.Precision

{- |
	* The type in which all arithmetic is performed.

	* A small dynamic range, 32 bits or more, is typically adequate.
-}
type I	= Int

-- | The constant base in which we want the resulting value of /Pi/ to be expressed.
decimal :: I
decimal	= 10

-- | Coerce the polymorphic type 'Data.Ratio.Ratio' to suit the base used in our series.
type Base	= Data.Ratio.Ratio I

-- | Coerce the polymorphic type returned by 'quotRem' to our specific requirements.
type QuotRem	= (I, I)

-- Accessors.
getQuotient, getRemainder :: QuotRem -> I
getQuotient	= fst
getRemainder	= snd

type PreDigits		= [I]
type Pi			= [I]
type Coefficients	= [I]

{- |
	* For a digit on one row of the spigot-table, add any numerator carried from the similar calculation one column to the right.

	* Divide the result of this summation, by the denominator of the base, to get the quotient and remainder.

	* Determine the quantity to carry to the similar calculation one column to the left, by multiplying the quotient by the numerator of the base.
-}
carryAndDivide :: (Base, I) -> QuotRem -> QuotRem
carryAndDivide (base, lhs) rhs
	| n < d		= (0, n)	-- In some degenerate cases, the result of the subsequent calculation can be more simply determined.
	| otherwise	= Control.Arrow.first (* Data.Ratio.numerator base) $ n `quotRem` d
	where
		d, n :: I
		d	= Data.Ratio.denominator base
		n	= lhs + getQuotient rhs	-- Carry numerator from the column to the right and add it to the current digit.

{- |
	* Fold 'carryAndDivide', from right to left, over the columns of a row in the spigot-table, continuously checking for overflow.

	* Release any previously withheld result-digits, after any adjustment for overflow in the current result-digit.

	* Withhold the current result-digit until the risk of overflow in subsequent result-digits has been assessed.

	* Call 'mkRow'.
-}
processColumns
	:: Math.Implementations.Pi.Spigot.Series.Series I
	-> PreDigits
	-> [(Base, I)]	-- ^ Data-row.
	-> Pi
processColumns series preDigits l
	| overflowMargin > 1	= preDigits ++ nextRow [digit]				-- There's neither overflow, nor risk of impact from subsequent overflow.
	| overflowMargin == 1	= nextRow $ preDigits ++ [digit]			-- There's no overflow, but risk of impact from subsequent overflow.
	| otherwise		= map ((`rem` decimal) . succ) preDigits ++ nextRow [0]	-- Overflow => propagate the excess to previously withheld preDigits.
	where
		results :: [QuotRem]
		results	= init $ scanr carryAndDivide (0, undefined) l

		digit :: I
		digit	= getQuotient $ head results

		overflowMargin :: I
		overflowMargin	= decimal - digit

		nextRow :: [I] -> [I]
		nextRow preDigits'	= mkRow series preDigits' $ map getRemainder results

{- |
	* Multiply the remainders from the previous row.

	* Zip them with the constant bases, with an addition one stuck on the front to perform the conversion to decimal, to create a new row of the spigot-table.

	* Call 'processColumns'.
-}
mkRow :: Math.Implementations.Pi.Spigot.Series.Series I -> PreDigits -> Coefficients -> Pi
mkRow series preDigits	= processColumns series preDigits . zip (recip (fromIntegral decimal) : Math.Implementations.Pi.Spigot.Series.bases series) . map (* decimal)

{- |
	* Initialises a /spigot/-table with the row of 'Math.Implementations.Pi.Spigot.Series.coefficients'.

	* Ensures that the row has suffient terms to accurately generate the required number of digits.

	* Extracts only those digits which are guaranteed to be accurate.

	* CAVEAT: the result is returned as an 'Integer', i.e. without any decimal point.
-}
openI :: Math.Implementations.Pi.Spigot.Series.Series I -> Math.Precision.DecimalDigits -> Integer
openI series decimalDigits	= read . map (
	Data.Char.intToDigit . fromIntegral
 ) . take decimalDigits . mkRow series [] . take (
	Math.Implementations.Pi.Spigot.Series.nTerms series decimalDigits
 ) $ Math.Implementations.Pi.Spigot.Series.coefficients series

