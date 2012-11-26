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
module TRS.Rule (
          Rule(..)
        , amendFuncVarR
        , amendFuncVarRs
    )where

import TRS.Term

data Rule = Rule {lhs::Term, rhs::Term}

-- show a rule as follows: lhs -> rhs
instance Show Rule where
    show r = show (lhs r) ++ " -> " ++ show (rhs r)

-- | 'amendFuncVarR' @r vs@ takes a rule and a list of terms and converts every 'Var' occuring in the given whichs name occurs
-- in @vs@ to a 0-ary function with the same name
amendFuncVarR :: Rule -> [Term] -> Rule
amendFuncVarR r vs = Rule (amendFuncVarT (lhs r) vs) (amendFuncVarT (rhs r) vs)
-- | 'amendFuncVarRs' @rs vs@ works like 'amendFuncVarR' but takes a list of rules
amendFuncVarRs :: [Rule] -> [Term] -> [Rule]
amendFuncVarRs rs vs = map (flip amendFuncVarR $ vs) rs
