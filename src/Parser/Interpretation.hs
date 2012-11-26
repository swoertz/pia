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
module Parser.Interpretation
    (
      parseInterpretation
    ) where

import Text.ParserCombinators.Parsec
import Parser.Util
import Parser.Polynomial (polynomial)
import Polynomial (dimension)
import Interpretation

-- | 'parseInterpretation' takes a string containing an interpretation and tries to parse it.
-- an polynomial interpretation containing the two function @s(x)@ and @0@ may look like this
--
-- >[s](x) = [[1]]*x + [[2]]
-- >[0]() = [[2]]
--
parseInterpretation :: String -> Interpretation
parseInterpretation content = case parse interpretation "interpretation file" content of
    Left err -> error . show $ err
    Right int -> int

interpretation :: Parser Interpretation
interpretation = do
    spaces
    fs <- Parser.Interpretation.function `sepBy1` spaces
    let dim = Polynomial.dimension . Interpretation.polynomial . head $ fs
    return (Interpretation dim fs)

function :: Parser Function
function = do
    spaces
    _ <- char '['
    functionName <- identifier
    _ <- char ']'
    spaces
    _ <- char '('
    spaces
    arguments <- identifier `sepBy` try (spaces >> char ',')
    _ <- char ')'
    spaces
    _ <- char '='
    spaces
    poly <- Parser.Polynomial.polynomial
    return (Interpretation.function functionName arguments poly)



