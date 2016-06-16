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

	* Exports a common interface for primality-implementations.

	* Provides utilities for these implementations.
-}

module Factory.Math.Primality(
-- * Type-classes
	Algorithmic(..),
-- * Functions
	carmichaelNumbers,
-- ** Predicates
	areCoprime,
	isFermatWitness,
	isCarmichaelNumber
) where

import qualified	Control.DeepSeq
import qualified	Factory.Math.Power	as Math.Power

-- | Defines the methods expected of a primality-testing algorithm.
class Algorithmic algorithm	where
	isPrime	:: (Control.DeepSeq.NFData i, Integral i, Show i) => algorithm -> i -> Bool

{- |
	'True' if the two specified integers are /relatively prime/,
	i.e. if they share no common positive factors except one.

	* @1@ and @-1@ are the only numbers which are /coprime/ to themself.

	* <https://en.wikipedia.org/wiki/Coprime>.

	* <http://mathworld.wolfram.com/RelativelyPrime.html>.
-}
areCoprime :: Integral i => i -> i -> Bool
areCoprime i	= (== 1) . gcd i

{- |
	* Tests /Fermat's Little Theorem/ for all applicable values, as a probabilistic primality-test.

	* <https://en.wikipedia.org/wiki/Fermat%27s_little_theorem>.

	* <https://en.wikipedia.org/wiki/Fermat_primality_test>.

	* <https://en.wikipedia.org/wiki/Fermat_pseudoprime>.

	* CAVEAT: this primality-test fails for the /Carmichael numbers/.

	* TODO: confirm that all values must be tested.
-}
isFermatWitness :: (Integral i, Show i) => i -> Bool
isFermatWitness i	= not . all isFermatPseudoPrime $ filter (areCoprime i) [2 .. pred i]	where
	isFermatPseudoPrime base	= Math.Power.raiseModulo base (pred i) i == 1	-- CAVEAT: a /Fermat Pseudo-prime/ must also be a /composite/ number.

{- |
	* A /Carmichael number/ is an /odd/ /composite/ number which satisfies /Fermat's little theorem/.

	* <https://en.wikipedia.org/wiki/Carmichael_number>.

	* <http://mathworld.wolfram.com/CarmichaelNumber.html>.
-}
isCarmichaelNumber :: (
	Algorithmic		algorithm,
	Control.DeepSeq.NFData	i,
	Integral		i,
	Show			i
 ) => algorithm -> i -> Bool
isCarmichaelNumber algorithm i	= not $ or [
	i <= 2,
	even i,
	isFermatWitness i,
	isPrime algorithm i
 ]

-- | An ordered list of the /Carmichael/ numbers; <https://en.wikipedia.org/wiki/Carmichael_number>.
carmichaelNumbers :: (
	Algorithmic		algorithm,
	Control.DeepSeq.NFData	i,
	Integral		i,
	Show			i
 ) => algorithm -> [i]
carmichaelNumbers algorithm	= isCarmichaelNumber algorithm `filter` [3, 5 ..]
