import VaryRPN
import System.Environment

main :: IO()
main = do
  args <- getArgs
  if null args
  then putStrLn "Usage: puzzler022018 digits"
  else putStrLn.unlines.describeSolutions $ head args

binops :: [Binop]
binops = "+-*/"
         
rpnEqual100 :: RPN -> Bool
rpnEqual100 = (==100).solveRPN

rpnSolutions :: [Digit] -> [RPN]
rpnSolutions = (filter rpnEqual100).(map pnToRPN).(pnVariations binops).reverse

describeSolutions :: [Digit] -> [String]
describeSolutions digits = msg1:msg2:"":(map biformat) sols
    where sols = rpnSolutions digits
          nsol = (show.length) sols
          msg1 = unwords [nsol, "expressions of the sequence", digits, "equal 100."]
          msg2 = "They are, in reverse polish and parenthetical notations:"
          biformat rpn = rpn ++ "\t" ++ rpnToParen binops rpn
