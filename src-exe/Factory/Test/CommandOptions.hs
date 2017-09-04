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

 [@DESCRIPTION@]	Defines the available set of command-line options; of which there's currently only one.
-}

module Factory.Test.CommandOptions(
-- * Types
-- ** Data-types
	CommandOptions(..),
-- * Functions
-- ** Mutators
	setVerbose
) where

import qualified	Data.Default

-- | Declare a record used to contain command-line options.
newtype CommandOptions	= MkCommandOptions {
	verbose	:: Bool	-- ^ Whether additional informative output should be generated, where applicable.
}

instance Data.Default.Default CommandOptions	where
	def	= MkCommandOptions { verbose = False }

-- | Mutator.
setVerbose :: CommandOptions -> CommandOptions
setVerbose commandOptions = commandOptions {
	verbose	= True
}

