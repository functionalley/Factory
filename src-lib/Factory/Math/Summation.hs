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

 [@DESCRIPTION@]	Provides an alternative algorithm for the summation of /rational/ numbers.
-}

module Factory.Math.Summation(
-- * Functions
	sum',
	sumR',
	sumR
) where

import qualified	Control.DeepSeq
import qualified	Control.Parallel.Strategies
import qualified	Data.List
import qualified	Data.Ratio
import			Data.Ratio((%))
import qualified	ToolShed.Data.List

{- |
	* Sums a list of numbers of arbitrary type.

	* Sparks the summation of @(list-length / chunk-size)@ chunks from the list, each of the specified size (thought the last chunk may be smaller),
	then recursively sums the list of results from each spark.

	* CAVEAT: unless the numbers are large, 'Rational' (requiring /cross-multiplication/), or the list long,
	'sum' is too light-weight for sparking to be productive,
	therefore it is more likely to be the parallelised deep /evaluation/ of list-elements which saves time.
-}
sum' :: (Num n, Control.DeepSeq.NFData n)
	=> ToolShed.Data.List.ChunkLength
	-> [n]
	-> n
sum' chunkLength
	| chunkLength <= 1	= error $ "Factory.Math.Summation.sum':\tinvalid chunk-size; " ++ show chunkLength
	| otherwise		= slave
	where
		slave :: (Num n, Control.DeepSeq.NFData n) => [n] -> n
		slave []	= 0
		slave [x]	= x
		slave l		= slave {-recurse-} . Control.Parallel.Strategies.parMap Control.Parallel.Strategies.rdeepseq sum $ ToolShed.Data.List.chunk chunkLength l

{- |
	* Sums a list of /rational/ type numbers.

	* CAVEAT: though faster than 'Data.List.sum', this algorithm has poor space-complexity, making it unsuitable for unrestricted use.
-}
{-# INLINE sumR' #-}	-- This makes a staggering difference.
sumR' :: Integral i => [Data.Ratio.Ratio i] -> Data.Ratio.Ratio i
sumR' l	= foldr (\ratio -> ((Data.Ratio.numerator ratio * (commonDenominator `div` Data.Ratio.denominator ratio)) +)) 0 l % commonDenominator	where
--	commonDenominator	= foldr (lcm . Data.Ratio.denominator) 1 l
	commonDenominator	= Data.List.foldl' (\multiple -> lcm multiple . Data.Ratio.denominator) 1 l	-- Slightly faster.

{- |
	* Sums a list of /rational/ numbers.

	* Sparks the summation of @(list-length / chunk-length)@ chunks from the list, each of the specified size (thought the last chunk may be smaller),
	then recursively sums the list of results from each spark.

	* CAVEAT: memory-use is proportional to chunk-size.
-}
{-# INLINE sumR #-}	-- This makes a staggering difference to calls from other modules.
sumR :: (Integral i, Control.DeepSeq.NFData i)
	=> ToolShed.Data.List.ChunkLength
	-> [Data.Ratio.Ratio i]
	-> Data.Ratio.Ratio i
sumR chunkLength
	| chunkLength <= 1	= error $ "Factory.Math.Summation.sumR:\tinvalid chunk-size; " ++ show chunkLength
	| otherwise		= slave
	where
		slave :: (Integral i, Control.DeepSeq.NFData i) => [Data.Ratio.Ratio i] -> Data.Ratio.Ratio i
		slave l
			| length l <= chunkLength	= sumR' l
			| otherwise			= slave {-recurse-} . Control.Parallel.Strategies.parMap Control.Parallel.Strategies.rdeepseq sumR' $ ToolShed.Data.List.chunk chunkLength l
