{-
    This file is part of pia.

    pia is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.

    pia is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with pia.  If not, see <http://www.gnu.org/licenses/>.
-}

{- |
Copyright   :  (c) Simon Woertz 2011-2012
Maintainer  :  Simon Woertz <simon@woertz.at>
Stability   :  provisional
-}
module Util where

import Data.List (nub)
import Numeric.LinearAlgebra
import Foreign.Storable(Storable)

-- | The 'YesNoMaybe' type encapsulates three different values 'Yes', 'No' and 'Maybe' 
data YesNoMaybe = Yes | No | Maybe deriving (Show, Eq)

-- | takes two 'YesNoMaybe' values and combines them as follows
-- 
-- > (//) Yes Yes = Yes
-- > (//) No _ = No
-- > (//) _ No = No
-- > (//) _ _ = Maybe
-- 
(//) :: YesNoMaybe -> YesNoMaybe -> YesNoMaybe
(//) Yes Yes = Yes
(//) No _ = No
(//) _ No = No
(//) _ _ = Maybe

-- | 'fillRight' @c n s@ adds as many times @c@ to the right side of the string, until the string @s@ has at least the given length @n@
fillRight :: Char -> Int -> String -> String
fillRight c n s = s ++ replicate (n - length s) c

-- | 'fillLeft' like 'fillRight', but this function adds the character @c@ to the left side of the string @s@
fillLeft :: Char -> Int -> String -> String
fillLeft c n s = replicate (n - length s) c ++ s

-- | 'fillCenter' like 'fillRight', but this function adds the character @c@ to the both sides of the string @s@
fillCenter :: Char -> Int -> String -> String
fillCenter c n s = replicate l c ++ s ++ replicate r c
    where x = n - length s
          l = x `div` 2
          r = x - l

-- | @'left' = 'fillLeft' ' '@
left :: Int -> String -> String
left = fillLeft ' '

-- | @'right' = 'fillRight' ' '@
right :: Int -> String -> String
right = fillRight ' '

-- | @'center' = 'fillCenter' ' '@
center :: Int -> String -> String
center = fillCenter ' '

-- | 'hasDuplicates' checks if a list has duplicate elements
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs = (length xs) /= (length $ nub xs)

-- | 'squareMatrix' takes a list of elements and converts it in to a square-matrix if the list has a suitable length
squareMatrix :: Storable a =>[a] -> Matrix a
squareMatrix xs = (width><width) xs where
    l = length xs
    s = (truncate . sqrt) (fromIntegral l ::Double)
    width | s * s == l = s
          | otherwise = error "inconsistent list size"

-- | 'beside' takes two 'String's and concatenates them line by line.
-- every line gets padded with spaces using 'adjustLineLength'
beside :: String -> String -> String
beside a b  = unlines' $ zipWith (++) (adjustLineLength linesA') linesB' where
    linesA = lines a
    linesB = lines b
    linesA' = replicate ((length linesB) - (length linesA)) "" ++ linesA
    linesB' = replicate ((length linesA) - (length linesB)) "" ++ linesB

-- | 'adjustLineLength' takes a list of lines and pads every line with spaces to get the line length of the longest
adjustLineLength :: [String] -> [String]
adjustLineLength ls = map (right maxLength) ls where
    lengths = map length ls
    maxLength = foldl1 max lengths

-- | 'unlines'' a list of strings like the one implemented in 'Prelude', but removes the newline-character at the end of the last line
unlines' :: [String] -> String
unlines' xs = take ((length ul) - 1) ul where
    ul = unlines xs
