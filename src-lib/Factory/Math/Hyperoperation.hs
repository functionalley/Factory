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

 [@DESCRIPTION@]	Provides various /hyperoperations/; <http://en.wikipedia.org/wiki/Hyperoperation>.
-}

module Factory.Math.Hyperoperation(
-- * Types
-- ** Type-synonyms
	Base,
	HyperExponent,
-- * Constants
	succession,
	addition,
	multiplication,
	exponentiation,
	tetration,
	pentation,
	hexation,
-- * Functions
	hyperoperation,
	ackermannPeter,
	powerTower,
-- ** Predicates
	areCoincidental
) where

import qualified	Data.List

{- |
	* Merely to enhance self-documentation.

	* CAVEAT: whilst it may appear that 'Base' could be non-'Integral', the recursive definition for /hyper-exponents/ above 'tetration', prevents this.
-}
type Base	= Integer

{- |
	* Merely to enhance self-documentation.

	* CAVEAT: whilst 'Base' and 'HyperExponent' can be independent types for both 'exponentiation' and 'tetration', they interact for other /hyper-exponents/.
-}
type HyperExponent	= Base

succession, addition, multiplication, exponentiation, tetration, pentation, hexation :: Int	-- Arbitrarily.
(succession : addition : multiplication : exponentiation : tetration : pentation : hexation : _)	= [0 ..]

{- |
	* Returns the /power-tower/ of the specified /base/; <http://mathworld.wolfram.com/PowerTower.html>.

	* A synonym for /tetration/;
		<http://en.wikipedia.org/wiki/Tetration>,
		<http://www.tetration.org/Fractals/Atlas/index.html>.
-}
powerTower :: (Integral base, Integral hyperExponent, Show base) => base -> hyperExponent -> base
powerTower 0 hyperExponent
	| even hyperExponent	= 1
	| otherwise		= 0
powerTower _ (-1)	= 0	-- The only negative hyper-exponent for which there's a consistent result.
powerTower base hyperExponent
	| base < 0 && hyperExponent > 1	= error $ "Factory.Math.Hyperoperation.powerTower:\tundefined for negative base; " ++ show base
	| otherwise			= Data.List.genericIndex (iterate (base ^) 1) hyperExponent

-- | The /hyperoperation/-sequence; <http://en.wikipedia.org/wiki/Hyperoperation>.
hyperoperation :: (Integral rank, Show rank) => rank -> Base -> HyperExponent -> Base
hyperoperation rank base hyperExponent
	| rank < fromIntegral succession	= error $ "Factory.Math.Hyperoperation.hyperoperation:\tundefined for rank; " ++ show rank
	| hyperExponent < 0			= error $ "Factory.Math.Hyperoperation.hyperoperation:\tundefined for hyper-exponent; " ++ show hyperExponent
	| otherwise				= rank ^# hyperExponent
	where
		(^#) :: Integral rank => rank -> HyperExponent -> Base
		r ^# 0	= case r of
			1 {-addition-}		-> base
			2 {-multiplication-}	-> 0
			_			-> 1
		r ^# e	= case r of
			0 {-succession-}	-> succ {-fromIntegral-} e
			1 {-addition-}		-> base + {-fromIntegral-} e
			2 {-multiplication-}	-> base * {-fromIntegral-} e
			3 {-exponentiation-}	-> base ^ e
			4 {-tetration-}		-> base `powerTower` e
			_
				| e' == e	-> tetration ^# e'	-- To which it would otherwise be reduced by laborious recursion.
				| otherwise	-> pred r ^# e'
				where
					e'	= {-fromIntegral $-} r ^# pred e

-- | The /Ackermann-Peter/-function; <http://en.wikipedia.org/wiki/Ackermann_function#Ackermann_numbers>.
ackermannPeter :: (Integral rank, Show rank) => rank -> HyperExponent -> Base
ackermannPeter rank	= (+ negate 3) . hyperoperation rank 2 {-base-} . (+ 3)

-- | True if @hyperoperation base hyperExponent@ has the same value for each specified 'rank'.
areCoincidental :: (Integral rank, Show rank) => Base -> HyperExponent -> [rank] -> Bool
areCoincidental _ _ []				= True
areCoincidental _ _ [_]				= True
areCoincidental base hyperExponent ranks	= all (== h) hs	where
	(h : hs)	= map (\rank -> hyperoperation rank base hyperExponent) ranks

