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
module TRS.Term (
          Term(..)
        , amendFuncVarT
    )where

data Term = 
    Var {name::String} |                                    --Variable with name
    Func {name::String, subterms::[Term]}                   --Function with name and arguments
    deriving Eq

instance Show Term where
    show (Var x) = x
    show (Func n []) =  n ++ "()"
    show (Func n (t:ts)) = n ++ "(" ++ show t ++ terms ts where
        terms [] = ")"
        terms (x:xs) = "," ++ show x ++ terms xs

-- | 'amendFuncVarT' takes a term and a list of variables. this function makes sure that term
-- contains only variables which are in the given list, otherwise it converts a variable to a zero-ary function
amendFuncVarT :: Term -> [Term] -> Term
amendFuncVarT t@(Var n) vs | t `elem` vs = t
                 | otherwise = Func n []
amendFuncVarT t@(Func n []) vs | (Var n) `elem` vs = Var n
                     | otherwise = t
amendFuncVarT (Func n ts) vs | (Var n) `elem` vs = error "cannot convert function with one or more arguments to variable"
                   | otherwise = Func n $ map (flip amendFuncVarT $ vs) ts

