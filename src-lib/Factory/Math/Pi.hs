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

 [@DESCRIPTION@]	Defines the classes of /Pi/-algorithm which have been implemented.
-}

module Factory.Math.Pi(
-- * Type-classes
	Algorithmic(..),
-- * Types
-- ** Data-types
	Category(..)
) where

import qualified	Factory.Math.Precision	as Math.Precision
import qualified	ToolShed.Defaultable

{- |
	* Defines the methods expected of a /Pi/-algorithm.

	* Most of the implementations naturally return a 'Rational', but the spigot-algorithms naturally produce a @[Int]@;
	though representing /Pi/ as a big integer with the decimal point removed is clearly incorrect.

	* Since representing /Pi/ as either a 'Rational' or promoted to an 'Integer', is inconvenient, an alternative decimal 'String'-representation is provided.
-}
class Algorithmic algorithm where
	openR	:: algorithm -> Math.Precision.DecimalDigits -> Rational	-- ^ Returns the value of /Pi/ as a 'Rational'.

	openI	:: algorithm -> Math.Precision.DecimalDigits -> Integer	-- ^ Returns the value of /Pi/, promoted by the required precision to form an integer.
	openI _ 1	= 3
	openI algorithm decimalDigits
		| decimalDigits <= 0	= error $ "Factory.Math.Pi.openI:\tinsufficient decimalDigits=" ++ show decimalDigits
		| otherwise		= round . Math.Precision.promote (openR algorithm decimalDigits) $ pred decimalDigits

	openS	:: algorithm -> Math.Precision.DecimalDigits -> String	-- ^ Returns the value of /Pi/ as a decimal 'String'.
	openS _ 1	= "3"
	openS algorithm decimalDigits
		| decimalDigits <= 0	= ""
		| decimalDigits <= 16	= take (succ decimalDigits) $ show (pi :: Double)
		| otherwise		= "3." ++ tail (show $ openI algorithm decimalDigits)	-- Insert a decimal point.

-- | Categorises the various algorithms.
data Category agm bbp borwein ramanujan spigot
	= AGM agm		-- ^ Algorithms based on the /Arithmetic-geometric Mean/.
	| BBP bbp		-- ^ <http://en.wikipedia.org/wiki/Bailey%E2%80%93Borwein%E2%80%93Plouffe_formula>.
	| Borwein borwein	-- ^ <http://en.wikipedia.org/wiki/Borwein%27s_algorithm>.
	| Ramanujan ramanujan	-- ^ <http://www.pi314.net/eng/ramanujan.php>.
	| Spigot spigot		-- ^ Algorithms from which the digits of /Pi/ slowly drip, one by one.
	deriving (Eq, Read, Show)

instance (
	ToolShed.Defaultable.Defaultable agm,
	ToolShed.Defaultable.Defaultable bbp,
	ToolShed.Defaultable.Defaultable borwein,
	ToolShed.Defaultable.Defaultable ramanujan,
	ToolShed.Defaultable.Defaultable spigot
 )  => ToolShed.Defaultable.Defaultable (Category agm bbp borwein ramanujan spigot)	where
	defaultValue	= BBP ToolShed.Defaultable.defaultValue

instance (
	Algorithmic agm,
	Algorithmic bbp,
	Algorithmic borwein,
	Algorithmic ramanujan,
	Algorithmic spigot
 ) => Algorithmic (Category agm bbp borwein ramanujan spigot)	where
	openR algorithm decimalDigits
		| decimalDigits <= 0	= error $ "Factory.Math.Pi.openR:\tinsufficient decimalDigits=" ++ show decimalDigits
		| decimalDigits <= 16	= Math.Precision.simplify (pred decimalDigits) (pi :: Double)
		| otherwise		= (
			case algorithm of
				AGM agm			-> openR agm
				BBP bbp			-> openR bbp
				Borwein borwein		-> openR borwein
				Ramanujan ramanujan	-> openR ramanujan
				Spigot spigot		-> openR spigot
		) decimalDigits

	openI _ 1				= 3
	openI (Spigot spigot) decimalDigits	= openI spigot decimalDigits
	openI algorithm decimalDigits
		| decimalDigits <= 0	= error $ "Factory.Math.Pi.openI:\tinsufficient decimalDigits=" ++ show decimalDigits
		| otherwise		= round . Math.Precision.promote (openR algorithm decimalDigits) $ pred decimalDigits

