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

 [@DESCRIPTION@]

	* Whilst this particular function is the subject of many introductory examples to Haskell,
	the simple algorithms appropriate for that forum, leave a large margin for performance-improvement.
	This module provides the interface for alternative algorithms.

	* <https://mathworld.wolfram.com/Factorial.html>.
-}

module Factory.Math.Factorial(
-- * Type-classes
	Algorithmic(..)
) where

-- | Defines the methods expected of a /factorial/-algorithm.
class Algorithmic algorithm	where
	factorial	:: (Integral i, Show i) => algorithm -> i -> i

