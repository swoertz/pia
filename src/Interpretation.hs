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
module Interpretation (
      Function(..)
    , Interpretation(..)
    , Orientation(..)
    , matchingVars
    , validArgs
    , orientateTerm
    , orientateRule
    , orientate
    , function
    , Interpretation.monotone
    , Interpretation.compatible
) where

import Polynomial
import Data.List (sort, find, intercalate)
import Util (beside, (//), hasDuplicates, YesNoMaybe)
import TRS as T hiding (name)
import Numeric.LinearAlgebra (ident)
import Numeric.LinearAlgebra.Util (zeros)

type Name = String
data Interpretation = Interpretation {dimension :: Int, functions :: [Function]}
instance Show Interpretation where
    show Interpretation {functions = fs} = concat $ map show fs

data Orientation = Orientation {lhs :: Polynomial, rhs :: Polynomial}
instance Show Orientation where
    {-
    show (Orientation l r) = (show . normalize $ l) `beside` " > " `beside`  (show . normalize $ r)
    -}
    show (Orientation l r) = show l' `beside` " > " `beside` show r' where
        l' = normalize $ subtractPolynomials l r
        r' = normalize $ subtractPolynomials r r

data Function = Function {
     name :: Name ,
     args :: [Var],
     polynomial :: Polynomial
     }

instance Show Function where
    show (Function fName arguments poly) = ("[" ++ fName ++ "](" ++ (intercalate "," arguments) ++ ") =") `beside` show poly

-- | 'function' creates a Function with the given name, variables and polynomial 
-- and ensures that there exists at least a 'Monomial' with a coefficient of zero for each variable
function :: Name -> [Var] -> Polynomial -> Function
function n vs p = Function n vs p' where
    p' = normalize $ p `addPolynomials`  linearFactors
    dim = Polynomial.dimension p
    linearFactors = Polynomial (map (\v -> Monomial (zeros dim dim) [v]) vs)

-- | 'matchingVars' checks if the arguments of a given 'Function' matches the variables of the given 'Polynomial'
matchingVars :: Function -> Bool
matchingVars (Function {args = a, polynomial = p}) 
    | (sort (varsPolynomial p)) == (sort a) = True
    | otherwise = False

-- | 'validArgs' returns 'True' if the given arguments are free of duplicates                             
validArgs :: Function -> Bool
validArgs (Function {args = as}) = not (hasDuplicates as)

-- | 'findFunction' @n i@ searches for a 'Function' with the name @n@ in the given 'Interpretation' @i@
-- and throws an error if no interpretation is found!
findFunction :: String -> Interpretation -> Function
findFunction n i = int where
    int = case find (\x -> n == (name x)) (functions i) of
                 Just x -> x
                 Nothing -> error $ "interpretation for " ++ n ++ " not found!"

-- | 'orientateTerm' takes a 'Term' and an 'Interpretation' and converts the term to a 'Polynomial' by substituting
-- the functions of the term into polynomial functions
orientateTerm ::  Term -> Interpretation -> Polynomial
orientateTerm (Var x) (Interpretation dim _) = Polynomial [Monomial (ident dim) [x]]
orientateTerm (Func n s) i = substitute (polynomial f') orientateArgs where
    f' = findFunction n i
    orientateArgs = zip (args f') (map (flip orientateTerm i) s)

-- | 'orientateRule' @rule i@ orientates both the left-hand-side and the ride-hand-side of the given rule depending on the
-- given orientation.
orientateRule :: Rule -> Interpretation -> Orientation
orientateRule rule i = Orientation lhs' rhs' where
    lhs' = orientateTerm (T.lhs rule) i
    rhs' = orientateTerm (T.rhs rule) i

-- | 'orientate' orientates every rule of the given 'TRS' using 'orientateRule'
orientate :: TRS -> Interpretation -> [Orientation]
orientate (TRS t) i = map (flip orientateRule i) t

-- | 'monotone' checks if every 'Function' in the given 'Interpretation' is monotone
-- it returns 'Yes' if every 'Function' is monontone, 'No' if any 'Function' is not monotone and otherwise 'Maybe'
monotone :: Interpretation -> YesNoMaybe
monotone (Interpretation {functions = fs}) = foldl1 (//) (map (Polynomial.monotone . polynomial)  fs)

-- | 'compatible' checks if every 'Orientation' in the given list is compatible
-- it returns 'Yes' if every 'Orientation' is compatible, 'No' if any 'Orientation' is not compatible and otherwise 'Maybe'
compatible :: [Orientation] -> YesNoMaybe
compatible os = foldl1 (//) (map (\o -> Polynomial.compatible ((Interpretation.lhs o) `subtractPolynomials` (Interpretation.rhs o)))  os)

