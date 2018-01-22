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
	along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}
{- |
 [@AUTHOR@]	Dr. Alistair Ward

 [@DESCRIPTION@]

	* Describes a <https://en.wikipedia.org/wiki/Univariate> polynomial and operations on it.

	* <https://en.wikipedia.org/wiki/Polynomial>.

	* <https://mathworld.wolfram.com/Polynomial.html>.
-}

module Factory.Data.Polynomial(
-- * Types
-- ** Type-synonyms
--	MonomialList,
-- ** Data-types
	Polynomial,
-- * Constants
	zero,
	one,
-- * Functions
	evaluate,
	getDegree,
	getLeadingTerm,
	lift,
	mod',
	normalise,
--	pruneCoefficients,
	raiseModulo,
	realCoefficientsToFrac,
	terms,
-- ** Constructors
	mkConstant,
	mkLinear,
	mkPolynomial,
-- ** Operators
	(*=),
-- ** Predicates
	areCongruentModulo,
	inAscendingOrder,
	inDescendingOrder,
--	inOrder,
	isMonic,
	isMonomial,
	isNormalised,
	isPolynomial,
--	isReduced,
	isZero
) where

import			Control.Arrow((&&&))
import qualified	Control.Arrow
import qualified	Data.List
import			Factory.Data.Monomial((<*>), (</>), (<=>), (=~))
import qualified	Factory.Data.Monomial		as Data.Monomial
import qualified	Factory.Data.QuotientRing	as Data.QuotientRing
import			Factory.Data.Ring((=*=), (=+=), (=-=))
import qualified	Factory.Data.Ring		as Data.Ring

#if MIN_VERSION_base(4,8,0)
import Prelude hiding ((<*>))	-- The "Prelude" from 'base-4.8' exports this symbol.
#endif

infixl 7 *=	-- Same as (*).

-- | The guts of a 'Polynomial'.
type MonomialList coefficient exponent	= [Data.Monomial.Monomial coefficient exponent]

{- |
	* The type of an arbitrary /univariate/ polynomial;
	actually it's more general, since it permits negative powers (<https://en.wikipedia.org/wiki/Laurent_polynomial>s).
	It can't describe /multivariate/ polynomials, which would require a list of /exponents/.
	Rather than requiring the /exponent/ to implement the /type-class/ 'Integral', this is implemented at the function-level, as required.

	* The structure permits gaps between /exponents/,
	in which /coefficients/ are inferred to be zero, thus enabling efficient representation of sparse polynomials.

	* CAVEAT: the 'MonomialList' is required to;
	be ordered by /descending/ exponent (ie. reverse <https://en.wikipedia.org/wiki/Monomial_order>);
	have had zero coefficients removed;
	and to have had /like/ terms merged;
	so the raw data-constructor isn't exported.
-}
newtype {- Integral exponent => -} Polynomial coefficient exponent	= MkPolynomial {
	getMonomialList	:: MonomialList coefficient exponent	-- ^ Accessor.
} deriving (Eq, Show)

-- | Makes /Polynomial/ a 'Data.Ring.Ring', over the /field/ composed from all possible /coefficients/; <https://en.wikipedia.org/wiki/Polynomial_ring>.
instance (
	Eq	c,
	Num	c,
	Num	e,
	Ord	e
 ) => Data.Ring.Ring (Polynomial c e) where
	MkPolynomial [] =*= _	= zero
	_ =*= MkPolynomial []	= zero
	polynomialL =*= polynomialR
--		| polynomialL == one			= polynomialR	-- Counterproductive.
--		| polynomialR == one			= polynomialL	-- Counterproductive.
		| terms polynomialL > terms polynomialR	= polynomialL `times` polynomialR
		| otherwise				= polynomialR `times` polynomialL
		where
			l `times` r	= {-# SCC "times" #-} Data.Ring.sum' (recip 2) {-TODO-} 10 {-empirical-} . map (l *=) $ getMonomialList r

	MkPolynomial [] =+= p				= p
	p =+= MkPolynomial []				= p
	MkPolynomial listL =+= MkPolynomial listR	= {-# SCC "merge" #-} MkPolynomial $ merge listL listR	where
		merge [] r			= r
		merge l []			= l
		merge l@(lh : ls) r@(rh : rs)	= case lh <=> rh of
			GT	-> lh : merge ls r
			LT	-> rh : merge l rs
			_	-> case lh `Data.Monomial.shiftCoefficient` Data.Monomial.getCoefficient rh of
				(0, _)		-> merge ls rs
				monomial	-> monomial : merge ls rs

	additiveInverse		= lift (Data.Monomial.negateCoefficient `map`)
	multiplicativeIdentity	= one
	additiveIdentity	= zero

{-
	Override the default implementation,
	in order to take advantage of the symmetry under reflection about the main diagonal,
	in the square matrix of products formed from the multiplication of each term by each term.
	Eg:
		(ax^3 + bx^2 + cx + d)^2 = [
			(a^2x^6 + abx^5 + acx^4 + adx^3) +
			(bax^5 + b^2x^4 + bcx^3 + bdx^2) +
			(cax^4 + cbx^3 + c^2x^2 + cdx) +
			(dax^3 + dbx^2 + dcx + d^2)
		]

		= (a^2x^6 + b^2x^4 + c^2x^2 + d^2) + 2 * [ax^3 * (bx^2 + cx + d) + bx^2 * (cx + d) + cx * (d)]
-}
	square (MkPolynomial [])	= zero
	square p			= Data.Ring.sum' (recip 2) {-TODO-} 10 {-empirical-} $ diagonal : corners	where
		diagonal	= {-# SCC "diagonal" #-} map Data.Monomial.square `lift` p
		corners		= {-# SCC "corners" #-} uncurry (
			zipWith (*=)
		 ) $ map MkPolynomial . init {-remove terminal null-} . Data.List.tails . tail &&& map Data.Monomial.double $ getMonomialList p

-- | Defines the ability to divide /polynomials/.
instance (
	Eq		c,
	Fractional	c,
	Num		e,
	Ord		e
 ) => Data.QuotientRing.QuotientRing (Polynomial c e)	where
{-
	Uses /Euclidian division/.
	<https://en.wikipedia.org/wiki/Polynomial_long_division>.
	<https://demonstrations.wolfram.com/PolynomialLongDivision/>.
-}
	_ `quotRem'` MkPolynomial []		= error "Factory.Data.Polynomial.quotRem':\tzero denominator."
	polynomialN `quotRem'` polynomialD	= longDivide polynomialN	where
--		longDivide :: (Fractional c, Num e, Ord e) => Polynomial c e -> (Polynomial c e, Polynomial c e)
		longDivide (MkPolynomial [])	= (zero, zero)	-- Exactly divides.
		longDivide numerator
			| Data.Monomial.getExponent quotient < 0	= (zero, numerator)	-- Indivisible remainder.
			| otherwise					= Control.Arrow.first (lift (quotient :)) $ longDivide (numerator =-= polynomialD *= quotient )
			where
--				quotient :: (Fractional c, Num e) => Data.Monomial.Monomial c e
				quotient	= getLeadingTerm numerator </> getLeadingTerm polynomialD

{- |
	* Transforms the data behind the constructor.

	* CAVEAT: similar to 'Data.Functor.fmap', but 'Polynomial' isn't an instance of 'Data.Functor.Functor' since we may want to operate on both /type-parameters/.

	* CAVEAT: the caller is required to re-'normalise' the resulting polynomial depending on the nature of the transformation of the data.
-}
lift :: (MonomialList c1 e1 -> MonomialList c2 e2) -> Polynomial c1 e1 -> Polynomial c2 e2
lift transform	= MkPolynomial . transform . getMonomialList

-- | Returns the number of non-zero terms in the polynomial.
terms :: Polynomial c e -> Int
terms (MkPolynomial l)	= length l

-- | Return the highest-degree monomial.
getLeadingTerm :: Polynomial c e -> Data.Monomial.Monomial c e
getLeadingTerm (MkPolynomial [])	= error "Factory.Data.Polynomial.getLeadingTerm:\tzero polynomial has no leading term."
getLeadingTerm (MkPolynomial (m : _))	= m

-- | Removes terms with a /coefficient/ of zero.
pruneCoefficients :: (Eq c, Num c) => Polynomial c e -> Polynomial c e
pruneCoefficients (MkPolynomial [])	= zero
pruneCoefficients p			= filter ((/= 0) . Data.Monomial.getCoefficient) `lift` p

-- | Sorts into /descending order/ of exponents, groups /like/ exponents, and calls 'pruneCoefficients'.
normalise :: (Eq c, Num c, Ord e) => Polynomial c e -> Polynomial c e
normalise	= pruneCoefficients . lift (
	map (
		foldr ((+) . Data.Monomial.getCoefficient) 0 &&& Data.Monomial.getExponent . head
	) . Data.List.groupBy (=~) . Data.List.sortBy (flip (<=>))
 )

-- | Constructs an arbitrary /zeroeth-degree polynomial/, ie. independent of the /indeterminate/.
mkConstant :: (Eq c, Num c, Num e) => c -> Polynomial c e
mkConstant 0	= zero
mkConstant c	= MkPolynomial [(c, 0)]

-- | Constructs an arbitrary /first-degree polynomial/.
mkLinear :: (Eq c, Num c, Num e)
	=> c	-- ^ Gradient.
	-> c	-- ^ Constant.
	-> Polynomial c e
mkLinear m c	= pruneCoefficients $ MkPolynomial [(m, 1), (c, 0)]

-- | Smart constructor. Constructs an arbitrary /polynomial/.
mkPolynomial :: (Eq c, Num c, Ord e) => MonomialList c e -> Polynomial c e
mkPolynomial []	= zero
mkPolynomial l	= normalise $ MkPolynomial l

-- | Constructs a /polynomial/ with zero terms.
zero :: Polynomial c e
zero	= MkPolynomial []

-- | Constructs a constant /monomial/, independent of the /indeterminate/.
one :: (Eq c, Num c, Num e) => Polynomial c e
one	= mkConstant 1

-- | True if all /exponents/ are in the order defined by the specified comparator.
inOrder :: (e -> e -> Bool) -> Polynomial c e -> Bool
inOrder comparator p
	| any ($ p) [isZero, isMonomial]	= True
	| otherwise				= and . uncurry (zipWith comparator) . (init &&& tail) . map Data.Monomial.getExponent $ getMonomialList p

-- | True if the /exponents/ of successive terms are in /ascending/ order.
inAscendingOrder :: Ord e => Polynomial c e -> Bool
inAscendingOrder	= inOrder (<=)

-- | True if the /exponents/ of successive terms are in /descending/ order.
inDescendingOrder :: Ord e => Polynomial c e -> Bool
inDescendingOrder	= inOrder (>=)

-- | True if no term has a /coefficient/ of zero.
isReduced :: (Eq c, Num c) => Polynomial c e -> Bool
isReduced	= all ((/= 0) . Data.Monomial.getCoefficient) . getMonomialList

-- | True if no term has a /coefficient/ of zero and the /exponents/ of successive terms are in /descending/ order.
isNormalised :: (Eq c, Num c, Ord e) => Polynomial c e -> Bool
isNormalised polynomial	= all ($ polynomial) [isReduced, inDescendingOrder]

{- |
	* 'True' if the /leading coefficient/ is one.

	* <https://en.wikipedia.org/wiki/Monic_polynomial#Classifications>.
-}
isMonic :: (Eq c, Num c) => Polynomial c e -> Bool
isMonic (MkPolynomial [])	= False	-- All coefficients are zero, and have therefore been removed.
isMonic p			= (== 1) . Data.Monomial.getCoefficient $ getLeadingTerm p

-- | True if there are zero terms.
isZero :: Polynomial c e -> Bool
isZero (MkPolynomial [])	= True
isZero _			= False

-- | True if there's exactly one term.
isMonomial :: Polynomial c e -> Bool
isMonomial (MkPolynomial [])	= True
isMonomial _			= False

-- | True if all /exponents/ are /positive/ integers as required.
isPolynomial :: Integral e => Polynomial c e -> Bool
isPolynomial	= all Data.Monomial.isMonomial . getMonomialList

{- |
	* 'True' if the two specified /polynomials/ are /congruent/ in /modulo/-arithmetic.

	* <https://planetmath.org/encyclopedia/PolynomialCongruence.html>.
-}
areCongruentModulo :: (Integral c, Num e, Ord e)
	=> Polynomial c e	-- ^ LHS.
	-> Polynomial c e	-- ^ RHS.
	-> c			-- ^ Modulus.
	-> Bool
areCongruentModulo _ _ 0	= error "Factory.Data.Polynomial.areCongruentModulo:\tzero modulus."
areCongruentModulo _ _ 1	= True
areCongruentModulo l r	modulus
	| l == r	= True
	| otherwise	= all ((== 0) . (`mod` modulus) . Data.Monomial.getCoefficient) . getMonomialList $ l =-= r

{- |
	* Return the /degree/ (AKA /order/) of the /polynomial/.

	* <https://en.wikipedia.org/wiki/Degree_of_a_polynomial>.

	* <https://mathworld.wolfram.com/PolynomialDegree.html>.
-}
getDegree :: Num e => Polynomial c e -> e
getDegree (MkPolynomial [])	= -1	-- CAVEAT: debatable, but makes some operations more robust and consistent.
getDegree p			= Data.Monomial.getExponent $ getLeadingTerm p

{- |
	* Scale-up the specified /polynomial/ by a constant /monomial/ factor.

	* <https://en.wikipedia.org/wiki/Scalar_multiplication>.
-}
(*=) :: (Eq c, Num c, Num e) => Polynomial c e -> Data.Monomial.Monomial c e -> Polynomial c e
polynomial *= monomial
	| Data.Monomial.getCoefficient monomial == 1	= map (`Data.Monomial.shiftExponent` Data.Monomial.getExponent monomial) `lift` polynomial
	| otherwise					= map (monomial <*>) `lift` polynomial

{- |
	* Raise a /polynomial/ to the specified positive integral power, but using /modulo/-arithmetic.

	* Whilst one could naively implement this as @(x Data.Ring.=^ n) `mod` m@, this will result in arithmetic operatons on unnecessarily big integers.
-}
raiseModulo :: (Integral c, Integral power, Num e, Ord e, Show power)
	=> Polynomial c e	-- ^ The base.
	-> power		-- ^ The exponent to which the base should be raised.
	-> c			-- ^ The modulus.
	-> Polynomial c e	-- ^ The result.
raiseModulo _ _ 0			= error "Factory.Data.Polynomial.raiseModulo:\tzero modulus."
raiseModulo _ _ 1			= zero
raiseModulo _ 0 modulus			= mkConstant $ 1 `mod` modulus
raiseModulo polynomial power modulus
	| power < 0			= error $ "Factory.Data.Polynomial.raiseModulo:\tthe result isn't guaranteed to be a polynomial, for power=" ++ show power
	| first `elem` [zero, one]	= first	-- Eg 'raiseModulo (mkPolynomial [(3,1)]) 100 3' or 'raiseModulo (mkPolynomial [(3,1),(1,0)]) 100 3'.
	| otherwise			= slave power
	where
--		first :: Integral c => Polynomial c e
		first	= polynomial `mod'` modulus

--		slave :: (Integral c, Integral power, Num e, Ord e) => power -> Polynomial c e
		slave 1	= first
		slave n	= (`mod'` modulus) . (if r == 0 {-even-} then id else (polynomial =*=)) . Data.Ring.square $ slave q {-recurse-}	where
			(q, r)	= n `quotRem` 2

-- | Reduces all the coefficients using /modular/ arithmetic.
mod' :: Integral c
	=> Polynomial c e
	-> c	-- ^ Modulus.
	-> Polynomial c e
mod' p modulus	= pruneCoefficients $ map (`Data.Monomial.mod'` modulus) `lift` p

{- |
	* Evaluate the /polynomial/ at a specific /indeterminate/.

	* CAVEAT: requires positive exponents; but it wouldn't really be a /polynomial/ otherwise.

	* If the /polynomial/ is very sparse, this may be inefficient,
	since it /memoizes/ the complete sequence of powers up to the polynomial's /degree/.
-}
evaluate :: (Num n, Integral e, Show e)
	=> n	-- ^ The /indeterminate/.
	-> Polynomial n e
	-> n	-- ^ The Result.
evaluate x	= foldr ((+) . raise) 0 . getMonomialList	where
	powers	= iterate (* x) 1

	raise monomial
		| exponent' < 0	= error $ "Factory.Data.Polynomial.evaluate.raise:\tnegative exponent; " ++ show exponent'
		| otherwise	= Data.Monomial.getCoefficient monomial * Data.List.genericIndex powers exponent'
		where
			exponent'	= Data.Monomial.getExponent monomial

-- | Convert the type of the /coefficient/s.
realCoefficientsToFrac :: (Real r, Fractional f) => Polynomial r e -> Polynomial f e
realCoefficientsToFrac	= lift (Data.Monomial.realCoefficientToFrac `map`)

