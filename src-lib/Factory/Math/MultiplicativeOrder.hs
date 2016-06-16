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

 [@DESCRIPTION@]	Exports the /Multiplicative Order/ of an integer, in a specific /modular/ arithmetic.

-}

module Factory.Math.MultiplicativeOrder(
-- * Functions
	multiplicativeOrder
) where

import qualified	Control.DeepSeq
import qualified	Factory.Data.Exponential	as Data.Exponential
import qualified	Factory.Math.Power		as Math.Power
import qualified	Factory.Math.Primality		as Math.Primality
import qualified	Factory.Math.PrimeFactorisation	as Math.PrimeFactorisation

{- |
	* The smallest positive integral power to which the specified integral base must be raised,
	to be congruent with one, in the specified /modular/ arithmetic.

	* Based on <http://rosettacode.org/wiki/Multiplicative_order#Haskell>.

	* <https://en.wikipedia.org/wiki/Multiplicative_order>.

	* <http://mathworld.wolfram.com/MultiplicativeOrder.html>.
-}
multiplicativeOrder :: (Math.PrimeFactorisation.Algorithmic primeFactorisationAlgorithm, Control.DeepSeq.NFData i, Integral i, Show i)
	=> primeFactorisationAlgorithm
	-> i	-- ^ Base.
	-> i	-- ^ Modulus.
	-> i	-- ^ Result.
multiplicativeOrder primeFactorisationAlgorithm base modulus
	| modulus < 2					= error $ "Factory.Math.MultiplicativeOrder.multiplicativeOrder:\tinvalid modulus; " ++ show modulus
	| not $ Math.Primality.areCoprime base modulus	= error $ "Factory.Math.MultiplicativeOrder.multiplicativeOrder:\targuments aren't coprime; " ++ show (base, modulus)
	| otherwise					= foldr (lcm . multiplicativeOrder') 1 $ Math.PrimeFactorisation.primeFactors primeFactorisationAlgorithm modulus	-- Combine the /multiplicative order/ of the constituent /prime-factors/.
	where
--		multiplicativeOrder' :: (Control.DeepSeq.NFData i, Integral i) => Data.Exponential.Exponential i -> i
		multiplicativeOrder' e	= product . map (
			\e'	-> let
				d :: Int
				d	= length . takeWhile (/= 1) . iterate (
					\y	-> Math.Power.raiseModulo y (Data.Exponential.getBase e') pk
				 ) $ Math.Power.raiseModulo base (totient `div` Data.Exponential.evaluate e') pk
			in Data.Exponential.getBase e' ^ d
		 ) $ Math.PrimeFactorisation.primeFactors primeFactorisationAlgorithm totient	where
			pk	= Data.Exponential.evaluate e
			totient	= Math.PrimeFactorisation.primePowerTotient e

