{-|
Description : Evaluate a sequence of digits for all binary operator associations.
Copyright   : (c) Burton Betchart, 2018
License     : CC BY-NC-SA 3.0
              https://creativecommons.org/licenses/by-nc-sa/3.0/

The goal is to find binary operator associations for a given sequence
of operands such that the evaluation of the expression meets a given
criteria.

Polish notation (PN) relies on the ordering of operators and operands
rather than parentheses to specify operator application. Re-ordering
of operators and operands is more straightforward than modifying
parenthetical expressions, so PN is a good choice for the manipulation
of operator application to a sequence of digits.

For strictly binary operators, a valid expression in PN always ends
with an operand, and any prefix of the expression (up to but not
including the final operand) always contains at least as many
operators as operands (every such prefix is a Dyck word).

Given a valid PN expression of n operands, a valid PN expression of
n+1 operands can be constructed by prepending an operator-operand
pair.  Other valid PN expressions containing the same sequence of n+1
operands and the same sequence of n operators can be obtained by
swapping the first operand with any subsequent operator which preceeds
the next operand.

The set of all valid PN expressions containing a given sequence of n+1
operands and n operators can be built recursively by prepending the
first operator-operand pair to each valid PN expression containing the
remaining sequences of n operands and n-1 operators, and swapping the
first operand with subsequent operators preceeding the next operand.
The recursion terminates with a valid PN expression consisting of just
the last operand.

Using a single binary operator, the number of valid expressions
containing a sequence of n+1 operands and n operators is the nth
Catalan number, C_n.  The first Catalan numbers for n=0,1,2,3,... are
1,1,2,5,14,132,...
https://en.wikipedia.org/wiki/Catalan_number

For a set of m operators, the number of valid expressions containing a
sequence of n+1 operands and n operators is the nth Catalan number
multiplied by m to the nth power, C_n * m^n.  For example, the number
of PN expressions containing 7 operands and 6 operators from the set
{+,-,*,/} is 132 * 4^6 = 540672.

Polish notation is often reversed (RPN) because it is easier to
calculate left-to-right, and has the same sequence of operators and
operands as a parenthetical expression.

-}
module VaryRPN (PN, RPN, Digit, Binop,
                pnToRPN, pnVariations, solveRPN, rpnToParen
               ) where

import Data.List (intersperse)

type Binop = Char -- + - * /
type Digit = Char -- Base 10 digits, 0 through 9
type PN = String  -- Polish Notation (no whitespace, single digit operands)
type RPN = String -- Reverse Polish Notation (whitespace separates operands, operators)

pnToRPN :: PN -> RPN
pnToRPN = reverse . intersperse ' '

-- |Valid PN variations of a sequence of single digit operands using a
-- given set of Binop characters
pnVariations :: [Binop] -> [Digit] -> [PN]
pnVariations _ [] = []
pnVariations _ (n:[]) = [n:[]]
pnVariations ops (n:ns) = concat [perms (op:n:subPN) |
                                  op <- ops, subPN <- pnVariations ops ns]
    where perms (n:[]) = [n:[]]
          perms s@(op:n:c:cs) = s:swaps
              where swaps = if elem c ops
                            then [op:v | v <- perms (c:n:cs)]
                            else []

-- |Reverse Polish Notation solver courtesy of
-- http://learnyouahaskell.com/functionally-solving-problems
solveRPN :: RPN -> Float  
solveRPN = head . foldl foldingFunction [] . words  
    where foldingFunction (x:y:ys) "*" = (x * y):ys
          foldingFunction (x:y:ys) "+" = (x + y):ys
          foldingFunction (x:y:ys) "-" = (y - x):ys
          foldingFunction (x:y:ys) "/" = (y / x):ys
          foldingFunction xs numberString = read numberString:xs

rpnToParen :: [Binop] -> RPN -> String
rpnToParen ops = head . foldl foldingFunction [] . words
    where foldingFunction (x:y:ys) s
              | elem (head s) ops = ("(" ++ y ++ s ++ x ++ ")"):ys
          foldingFunction xs numberString = numberString:xs
