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
module Parser.Polynomial (
    polynomial
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number (sign, natFloat)
import Polynomial (Monomial(..), Polynomial(..))
import Numeric.LinearAlgebra (Matrix, fromLists, ident)
import Parser.Util 

-- | 'row' parses positive/negative integer and floating point numbers separated by @','@ (commas) and surrounded by brackets
-- and returns a list of 'Double' values
row :: Parser [Double]
row = do
    spaces
    _ <- char '['
    spaces  
    v <- signedNumber `sepBy` (spaces >> char ',' >> spaces)
    spaces
    _ <- char ']'
    spaces
    return v

-- | 'signedNumber' parses a (un)signed integer or floating-point number and returns a it as 'Double'
signedNumber :: Parser Double
signedNumber = do
    spaces
    s <- sign
    spaces
    n <- natFloat
    return $ s (either fromInteger id n)

-- | 'matrix' parses a nXm-matrix of the form @[[x11,..,x1m];..;[xn1,..,xnm]]@
matrix :: Parser (Matrix Double)
matrix = do
    _ <- char '['
    spaces
    vectors <- row `sepBy` (spaces >> char ';')
    spaces
    _ <- char ']'
    return (fromLists vectors)

coefficient :: Parser (Matrix Double)
coefficient = do
   matrix <|> oneDimCoeff

oneDimCoeff:: Parser (Matrix Double)
oneDimCoeff = do
    n <-signedNumber
    return (fromLists [[n]])

-- | 'monomial' parses a monomial i.e. of the form
-- @[[1,2];[3,4]]*x@
monomial :: Parser Monomial
monomial = do
           spaces
           c <- option (ident 1) coefficient <?> "coefficient"
           spaces
           vs <- option [] variables
           return (Monomial c vs)

positiveMonomial :: Parser Monomial
positiveMonomial = do
                spaces
                _ <- char '+'
                spaces
                monomial

negativeMonomial :: Parser Monomial
negativeMonomial = do
                spaces
                _ <- char '-'
                spaces
                m <- monomial
                let (Monomial c vs) = m
                return (Monomial (c * (-1)) vs)

variables :: Parser [String]
variables = do
    spaces
    optional (char '*')
    spaces
    identifier `sepBy` (spaces >> char '*') <?> "variable"

-- | 'polynomial' parses a polynomial i.e. of the form 
-- @[[1,2];[3,4]]*x + [[0];[1]]@
polynomial :: Parser Polynomial
polynomial = do
            spaces
            m <-  (positiveMonomial <|> negativeMonomial) <|> monomial
            ms <- (positiveMonomial <|> negativeMonomial) `sepBy` (spaces)
            return (Polynomial (m:ms))

