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
module Polynomial (
      Monomial(..)
    , Polynomial(..)
    , Var
    , Coeff
    , substitute
    , subtractPolynomials
    , addPolynomials
    , normalize
    , varsPolynomial
    , monotone
    , compatible
    , linear
    , dimension
) where
import Data.List(sort, sortBy, groupBy, group, union, find, intercalate)
import Numeric.LinearAlgebra(Matrix, mXm, ident, atIndex, minElement, rows, cols)
import Numeric.LinearAlgebra.Util(zeros)
import Util

type Coeff = Matrix Double
type Var = String

data Monomial = Monomial {coeff :: Coeff , vars :: [Var]}
newtype Polynomial = Polynomial [Monomial]

instance Show Monomial where
    show = showMonomial
showMonomial :: Monomial -> String
showMonomial (Monomial c v) = coefficient `beside` variables where
    variables | v == [] = ""
              | otherwise = "*" ++ intercalate "*" v
    coefficient = unlines (tail (lines . show $ c)) -- drop rol/col information of matrix

instance Eq Monomial where
    (==) (Monomial a _ ) (Monomial b _ ) | a == b && a == zeros (rows a) (cols a) = True
    (==) a b = ca == cb && a `sameVars` b
        where (Monomial ca _) = a
              (Monomial cb _) = b

instance Show Polynomial where
    show = showPolynomial
showPolynomial :: Polynomial -> String
showPolynomial (Polynomial []) = ""
showPolynomial (Polynomial p) = show' p' where
    show' [] = ""
    show' [m] = show m
    show' (m:ms) = (show m `beside` " +" )`beside` show' ms
    dim = dimension (Polynomial p)
    nonZero = filter (\m -> coeff m /= zeros dim dim && coeff m /= zeros dim 1) p
    p' | nonZero == [] = [zero dim]
       | otherwise = nonZero

{-
constant :: Monomial -> Bool
constant m | vars m == [] = True
           | otherwise    = False
-}

zero :: Int -> Monomial
zero dim = Monomial (zeros dim 1) []

sameVars :: Monomial -> Monomial -> Bool
sameVars (Monomial _ a) (Monomial _ b) | (sort a) == (sort b) = True
                                         | otherwise = False

addPolynomials :: Polynomial -> Polynomial -> Polynomial
addPolynomials (Polynomial a) (Polynomial b) = Polynomial (a ++ b)

-- | 'subtractPolynomials' takes two polynomials and returns a polynomial which is the subtraction of these two
subtractPolynomials :: Polynomial -> Polynomial -> Polynomial
subtractPolynomials (Polynomial a) (Polynomial b) = Polynomial (a ++ (negateMonomials b))

-- |'normalize' takes a polynomial and combines monomials with the same variables and the constant factors e.g.
--
-- >>> normalize (Polynomial [Monomial 1 ["x"], Monomial 3 [], Monomial 2 ["x"], Monomial 1 ["y"], Monomial (-2) []]
--  [ 1.0 ] + [ 3.0 ]*x + [ 1.0 ]*y
--
normalize :: Polynomial -> Polynomial
normalize (Polynomial ms) = Polynomial (map sumMonomials groups)
    where groups = groupBy sameVars $ sortBy variables (zero (dimension (Polynomial ms)):ms)
          variables (Monomial _ a) (Monomial _ b) = compare (sort a) (sort b)

-- | 'sumMononmials' takes a list of 'Monomial's and sums up the coeffiecients. This only works if every 'Monomial' contains
-- excactly the same variables or none if its a constant factor
sumMonomials :: [Monomial] -> Monomial
sumMonomials m = foldl1 addMonomials m


negateMonomial :: Monomial -> Monomial
negateMonomial (Monomial c vs) = (Monomial (negate c) vs)

negateMonomials :: [Monomial] -> [Monomial]
negateMonomials m = map negateMonomial m

addMonomials :: Monomial -> Monomial -> Monomial
addMonomials a b | sameVars a b = Monomial (ca + cb) va
                 | otherwise = error "different variables"
    where (Monomial ca va) = a
          (Monomial cb _) = b

multiplyMonomials :: Monomial -> Monomial -> Monomial
multiplyMonomials (Monomial ca va) (Monomial cb vb) = Monomial (ca `mXm` cb) (va ++ vb)

-- | 'multiplyPolynomials' multiplies every 'Monomial' of the first 'Polynomial' with every 'Monomial' of the second
-- 'Polynomial'
multiplyPolynomials :: Polynomial -> Polynomial -> Polynomial
multiplyPolynomials (Polynomial p1) (Polynomial p2) = Polynomial [a `multiplyMonomials` b | a <- p1, b <- p2]

varsMonomials :: [Monomial] -> [Var]
varsMonomials ((Monomial _ vs):ms) = vs `union` (varsMonomials ms)
varsMonomials [] = []

-- | 'varsPolynomial' takes a polynomial and returns a list of variables which are occur in the polynomial
varsPolynomial :: Polynomial -> [Var]
varsPolynomial (Polynomial ms)  = varsMonomials ms

-- | 'substitute' @sigma@ takes a polynomial and a map @sigma@ which contains the substitutions.
-- every variable gets substituted by the corresponding polynomial found in the map
substitute :: Polynomial -> [(Var, Polynomial)] -> Polynomial
substitute (Polynomial p) sigma= foldl1 (addPolynomials) (map substitueMonomial p) where
    substitueMonomial (Monomial c vs) = foldl (multiplyPolynomials) (Polynomial [Monomial c []]) (map (var2Poly) vs) where
        var2Poly var = maybe (Polynomial [Monomial (ident (dimension (Polynomial p))) [var]]) (snd) (findTuple var)
        findTuple var = Data.List.find ((var == ) . fst) sigma

-- | 'compatible' @p@ takes a polynomial and checks if it is compatible, which means there are no negative coefficients
-- in the polynomial and the constant factor is positive
compatible :: Polynomial -> YesNoMaybe
compatible p = case comp of
            True -> Yes
            False -> case linear p of
                True -> No
                False -> Maybe
        where
        comp = all criteria ms
        (Polynomial ms) = normalize p
        criteria (Monomial c []) = minElement c >= 0 && atIndex c (0,0) > 0 --constant
        criteria (Monomial c _) = minElement c >= 0                         --coefficient

-- | 'monotone' @p@ takes a polynomial and checks if its monotonically increasing
monotone :: Polynomial -> YesNoMaybe
monotone p = case m of
            True -> Yes
            False -> case linear p of
                True -> No
                False -> Maybe
        where
        m = all criteria ms
        (Polynomial ms) = normalize p
        criteria (Monomial c [_]) =  minElement c >= 0 && atIndex c (0,0) >= 1 -- linear coefficient
        criteria (Monomial c _) = minElement c >= 0                           --constant or non-linear coefficient

-- | 'dimension' @p@ takes a polynomial and returns the amount of rows (dimension of square matrices) of the first monomial
-- found in the polynomial
dimension :: Polynomial -> Int
dimension (Polynomial p) = rows matrix where
    first = head p
    matrix = coeff first

-- | 'linear' @p@ takes a polynomial and returns 'False' if the degree of the polynomial is greater then 1. Otherwise it returns 'True'
linear :: Polynomial -> Bool
linear (Polynomial p) = (maximum (lengths ++ [0])) < 2 where
    lengths = map (\(Monomial _ v) -> length v) p
