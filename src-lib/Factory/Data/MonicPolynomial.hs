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

	* Describes a /monic polynomial; <https://en.wikipedia.org/wiki/Monic_polynomial#Classifications>;
	ie. in which the /coefficient/ of the /leading term/ is one.
-}

module Factory.Data.MonicPolynomial(
-- * Types
-- ** Data-types
	MonicPolynomial(getPolynomial),	-- Hide the data-constructor.
-- * Functions
-- ** Constructor
	mkMonicPolynomial
) where

import qualified	Control.Arrow
import qualified	Factory.Data.Monomial		as Data.Monomial
import			Factory.Data.Polynomial((*=))
import qualified	Factory.Data.Polynomial		as Data.Polynomial
import qualified	Factory.Data.QuotientRing	as Data.QuotientRing
import			Factory.Data.Ring((=*=), (=+=), (=-=))
import qualified	Factory.Data.Ring		as Data.Ring
import qualified	ToolShed.Data.Pair

-- | A type of 'Data.Polynomial.Polynomial', in which the /leading term/ is required to have a /coefficient/ of one.
newtype MonicPolynomial c e	= MkMonicPolynomial {
	getPolynomial	:: Data.Polynomial.Polynomial c e
} deriving (Eq, Show)

-- | Smart constructor. Constructs an arbitrary /monic polynomial/.
mkMonicPolynomial :: (
	Eq	c,
	Num	c,
	Show	c,
	Show	e
 ) => Data.Polynomial.Polynomial c e -> MonicPolynomial c e
mkMonicPolynomial polynomial
	| not $ Data.Polynomial.isMonic polynomial	= error $ "Factory.Data.MonicPolynomial.mkMonicPolynomial:\tnot monic; " ++ show polynomial
	| otherwise					= MkMonicPolynomial polynomial

{-
	* This instance-declaration merely delegates to the 'Data.Polynomial.Polynomial' payload.

	* CAVEAT: it's not strictly an instance of this class, since the result of some methods isn't /monic/.
-}
instance (
	Eq	c,
	Num	c,
	Num	e,
	Ord	e,
	Show	c,
	Show	e
 ) => Data.Ring.Ring (MonicPolynomial c e)	where
	MkMonicPolynomial l =*= MkMonicPolynomial r	= MkMonicPolynomial $ l =*= r
	MkMonicPolynomial l =+= MkMonicPolynomial r	= mkMonicPolynomial $ l =+= r	-- CAVEAT: potentially non-monic.
--	additiveInverse (MkMonicPolynomial p)		= MkMonicPolynomial $ Data.Ring.additiveInverse p	-- CAVEAT: not monic !
	additiveInverse _				= error "Factory.Data.MonicPolynomial.additiveInverse:\tresult isn't monic"
	multiplicativeIdentity				= MkMonicPolynomial Data.Ring.multiplicativeIdentity
	additiveIdentity				= MkMonicPolynomial Data.Ring.additiveIdentity	-- CAVEAT: not monic !

-- Since the /leading term/ of the /denominator/ is one, the /coefficient/ isn't required to implement 'Fractional'.
instance (
	Eq	c,
	Num	c,
	Num	e,
	Ord	e,
	Show	c,
	Show	e
 ) => Data.QuotientRing.QuotientRing (MonicPolynomial c e)	where
	MkMonicPolynomial polynomialN `quotRem'` MkMonicPolynomial polynomialD	= ToolShed.Data.Pair.mirror MkMonicPolynomial $ longDivide polynomialN	where
--		longDivide :: (Num c, Num e, Ord e) => Polynomial c e -> (Polynomial c e, Polynomial c e)
		longDivide numerator
			| Data.Polynomial.isZero numerator || Data.Monomial.getExponent quotient < 0	= (Data.Polynomial.zero, numerator)
			| otherwise									= Control.Arrow.first (Data.Polynomial.lift (quotient :)) $ longDivide (numerator =-= polynomialD *= quotient)
			where
--				quotient :: Num e => Data.Monomial.Monomial c e
				quotient	= Data.Polynomial.getLeadingTerm numerator `Data.Monomial.shiftExponent` negate (Data.Monomial.getExponent $ Data.Polynomial.getLeadingTerm polynomialD)

