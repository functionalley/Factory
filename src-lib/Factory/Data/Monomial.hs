{-# LANGUAGE CPP #-}
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
	along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
{- |
 [@AUTHOR@]	Dr. Alistair Ward

 [@DESCRIPTION@]

	* Describes a <https://en.wikipedia.org/wiki/Monomial> and operations on it.

	* A /monomial/ is merely a /polynomial/ with a single non-zero term; cf. /Binomial/.
-}

module Factory.Data.Monomial(
-- * Types
-- ** Type-synonyms
	Monomial,
-- * Functions
	double,
	mod',
	negateCoefficient,
	realCoefficientToFrac,
	shiftCoefficient,
	shiftExponent,
	square,
-- ** Accessors
	getExponent,
	getCoefficient,
-- ** Operators
	(<=>),
	(</>),
	(<*>),	-- CAVEAT: this clashes with the Prelude from 'base-4.8'.
	(=~),
-- ** Predicates
	isMonomial
) where

import qualified	Control.Arrow

#if MIN_VERSION_base(4,8,0)
import Prelude hiding ((<*>))	-- The "Prelude" from 'base-4.8' exports this symbol.
#endif

infix 4 <=>	-- Same as (==).
infix 4 =~	-- Same as (==).
infixl 7 </>	-- Same as (/).
infixl 7 <*>	-- Same as (*).

{- |
	* The type of an arbitrary monomial.

	* CAVEAT: though a /monomial/ has an integral power, this contraint is only imposed at the function-level.
-}
type Monomial coefficient exponent	= (coefficient, exponent)

-- | Accessor.
{-# INLINE getCoefficient #-}
getCoefficient :: Monomial c e -> c
getCoefficient	= fst

-- | Accessor.
{-# INLINE getExponent #-}
getExponent :: Monomial c e -> e
getExponent	= snd

{- |
	* 'True' if the /exponent/ is both integral and non-/negative/.

	* CAVEAT: one can't even call this function unless the /exponent/ is integral.
-}
isMonomial :: Integral e => Monomial c e -> Bool
isMonomial	= (>= 0) . getExponent

-- | Compares the /exponents/ of the specified 'Monomial's.
{-# INLINE (<=>) #-}
(<=>) :: Ord e => Monomial c e -> Monomial c e -> Ordering
(_, l) <=> (_, r)	= l `compare` r

-- | True if the /exponents/ are equal.
(=~) :: Eq e => Monomial c e -> Monomial c e -> Bool
(_, l) =~ (_, r)	= l == r

-- | Multiply the two specified 'Monomial's.
{-# INLINE (<*>) #-}
(<*>) :: (Num c, Num e) => Monomial c e -> Monomial c e -> Monomial c e
(cL, eL) <*> (cR, eR)	= (cL * cR, eL + eR)

-- | Divide the two specified 'Monomial's.
(</>) :: (Eq c, Fractional c, Num e)
	=> Monomial c e	-- ^ Numerator.
	-> Monomial c e	-- ^ Denominator.
	-> Monomial c e
(cN, eN) </> (1, eD)	= (cN, eN - eD)
(cN, eN) </> (cD, eD)	= (cN / cD, eN - eD)

-- | Square the specified 'Monomial'.
square :: (Num c, Num e) => Monomial c e -> Monomial c e
square (c, e)	= (c ^ (2 :: Int), 2 * e)

-- | Double the specified 'Monomial'.
{-# INLINE double #-}
double :: Num c => Monomial c e -> Monomial c e
double (c, e)	= (2 * c, e)

-- | Shift the /coefficient/, by the specified amount.
{-# INLINE shiftCoefficient #-}
shiftCoefficient :: Num c
	=> Monomial c e
	-> c	-- ^ The magnitude of the shift.
	-> Monomial c e
-- m `shiftCoefficient` i	= Control.Arrow.first (+ i) m	-- CAVEAT: Too slow.
(c, e) `shiftCoefficient` i	= (c + i, e)

-- | Shift the /exponent/, by the specified amount.
{-# INLINE shiftExponent #-}
shiftExponent :: Num e
	=> Monomial c e
	-> e	-- ^ The magnitude of the shift.
	-> Monomial c e
-- m `shiftExponent` i	= Control.Arrow.second (+ i) m	-- CAVEAT: Too slow.
(c, e) `shiftExponent` i	= (c, e + i)

-- | Negate the coefficient.
negateCoefficient :: Num c => Monomial c e -> Monomial c e
negateCoefficient	= Control.Arrow.first negate

-- | Reduce the coefficient using /modular/ arithmetic.
{-# INLINE mod' #-}
mod' :: Integral c
	=> Monomial c e
	-> c	-- ^ Modulus.
	-> Monomial c e
monomial `mod'` modulus	= Control.Arrow.first (`mod` modulus) monomial

-- | Convert the type of the /coefficient/.
realCoefficientToFrac :: (Real r, Fractional f) => Monomial r e -> Monomial f e
realCoefficientToFrac	= Control.Arrow.first realToFrac

