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
module Parser.TRS
    (
      parseTRS
    ) where

import Text.ParserCombinators.Parsec
import Parser.Util
import TRS hiding (rules)

{-

grammar of TRS input files from http://www.lri.fr/~marche/tpdb/format.html

           spec ::=         (   decl ) spec | ɛ
           decl ::=         VAR   idlist | THEORY   listofthdecl | RULES   listofrules
                                    | STRATEGY   strategydecl | id   anylist
        anylist ::=         ɛ | id   anylist | string   anylist
                                  | (   anylist )   anylist | ,   anylist
         idlist ::=         ɛ | id   idlist
   listofthdecl ::=         ɛ | (   thdecl   )   listofthdecl
         thdecl ::=         id   idlist | EQUATIONS   eqlist
              eqlist ::=         ɛ | equation   eqlist
       equation ::=         term   ==   term
    listofrules ::=         ɛ | rule   listofrules
           rule ::=         term   ->   term | term   ->   term   '|'   condlist
                                    | term   ->=   term | term   ->=   term   '|'   condlist
       condlist ::=         cond | cond   ,   condlist
           cond ::=         term   ->   term | term   -><-   term
           term ::=         id | id   (   ) | id   (   termlist   )
       termlist ::=         term | term   ,   termlist
   strategydecl ::=         INNERMOST | OUTERMOST | CONTEXTSENSITIVE   csstratlist
    csstratlist ::=         ɛ | ( id   intlist )   csstratlist
        intlist ::=         ɛ | int   intlist

id are non-empty sequences of characters except space, '(', ')', '"' and ',' ; and excluding special sequences '->', '==', ->=, '-><-', '|' and keywords CONTEXTSENSITIVE, EQUATIONS, INNERMOST, OUTERMOST, RULES, STRATEGY, THEORY and VAR.

string are sequences of any characters between double quotes

int are non-empty sequences of digits

-}

function :: Parser Term
function = do
       n <- identifier
       _ <- char '('
       spaces
       sts <- term `sepBy` try (spaces >> char ',')
       spaces
       _ <- char ')'
       spaces
       return (Func n sts)
       <?> "function"

variable :: Parser Term
variable = do
           n <- identifier
           return (Var n)
           <?> "variable"

term :: Parser Term
term = do
       (try function)
       <|> variable
       <?> "term"

rule :: Parser Rule
rule = do
       l <- term <?> "left hand side of rule"
       _ <- string "->"
       r <- term <?> "right hand side of rule"
       return (Rule l r)

specification :: String -> Parser a -> Parser [a]
specification s p = do 
                spaces
                _ <- char '(' <?> "opening parenthesis of specification"
                spaces
                _ <- string s
                result <- many1 (try p)
                _ <-char ')' <?> "closing parenthesis of specification"
                return (result)

rules :: Parser [Rule]
rules = skipTill $ specification "RULES" rule

vars :: Parser [Term]
vars = option [] (try $ skipTill variables) where
    variables = specification "VAR" variable

amend :: Either ParseError [Rule] -> Either ParseError [Term] -> [Rule]
amend (Right rs) (Right vs) = amendFuncVarRs rs vs
amend (Left err1) (Left err2) = error (show err1 ++ show err2)
amend (Left err) _ = error . show $ err
amend _ (Left err) = error . show $ err

-- | 'parseTRS' @file@ parses the given input and returns a 'TRS'
parseTRS :: String -> TRS
parseTRS file = TRS (amend rs vs) where
    vs = parse vars "trs file" file
    rs = parse rules "trs file" file
