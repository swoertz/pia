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
module Parser.Util where

import Text.ParserCombinators.Parsec
import Data.Char (isSpace)

-- | 'skipTill' @p@ takes a parser and skips all characters until it reaches a position, from which the parser @p@ could
-- parse without failing. 'skiptTill' fails if it reaches the end of the string before finding this position
skipTill :: Parser a -> Parser a
skipTill p = manyTill anyChar (try (lookAhead p)) >> p

-- | 'identifier' parses the identifying names for variables and functions.
-- the result does not contain whitespace characters or one of these characters @,()\"\*+[]@, but the parser also
-- consumes the surrounding whitespace characters
identifier :: Parser String
identifier = do
            spaces
            idString <- many1 (try (satisfy (\c -> c `notElem` (",()\"" ++ "*+-[]") && not (isSpace c))))
            spaces
            return idString
