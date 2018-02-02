-- Lambda Calculus Interpreter, Version 0.1.3

import System.IO
import Data.Char
import System.Process
import GHC.IO.Exception

-- DATA DEFINITIONS

type Variable = (Char, Integer)

data Expr = Singleton Variable
          | Application Expr Expr
          | Lambda Variable Expr
          deriving Eq

instance Show Expr where
    show (Singleton (v, 0)) = [v]
    show (Singleton (v, 1)) = [v]
    show (Singleton (v, i)) = [v, '_'] ++ (show $ i - 1)
    show (Application e1@(Application _ _) e2) = (init $ show e1) ++ " " 
                                               ++ show e2 ++ ")"
    show (Application e1 e2) = "(" ++ (show e1) ++ " " ++ (show e2) ++ ")"
    show (Lambda v e@(Lambda _ _)) = "(λ" ++ (show (Singleton v)) 
                                   ++ (tail $ tail $ show e)
    show (Lambda v e) = "(λ" ++ (show (Singleton v)) ++ "." ++ show e ++ ")"

data Error = IllegalChar Char
           | ParseError
           deriving Eq

instance Show Error where
    show e = "Error: " ++ case e of
                           ParseError    -> "no parse"
                           IllegalChar c -> "illegal use of character '" 
                                            ++ (c:"'")

-- EVALUATOR

rename :: Variable -> Variable
rename = fmap (+1)

evalLambda :: Expr -> Expr

evalLambda (Application e1 e2) = case e1 of
  (Application (Singleton v) e) -> Application 
                                     (Application (Singleton v) $ evalLambda e)
                                     e2
  (Application _ _)             -> if (e1 == evalLambda e1)
                                     then Application e1 e2
                                     else evalLambda 
                                       $ Application (evalLambda e1) e2
  (Singleton v)                 -> Application (Singleton v) (evalLambda e2)
  (Lambda v e)                  -> evalLambda $ replaceByIn v (evalLambda e2) e

evalLambda (Lambda v e)        = (Lambda w 
                                   (evalLambda $ replaceByIn v (Singleton w) e))
                         where w = rename v
evalLambda (Singleton v)       = (Singleton v)

replaceByIn :: Variable -> Expr -> Expr -> Expr
replaceByIn v e (Singleton w)       = if v == w 
                                      then e
                                      else Singleton w
replaceByIn v e (Application e1 e2) = Application (replaceByIn v e e1)
                                                  (replaceByIn v e e2)
replaceByIn v e (Lambda w e1)       = if (Singleton w) `isIn` e
                                      then doit v e $ Lambda (rename w)
                                              (replaceByIn w 
                                                           (Singleton 
                                                             $ rename w) 
                                                           e1)
                                      else doit v e (Lambda w e1)
        where doit v e (Lambda w e1)  = if v == w 
                                        then 
                                          Lambda w1
                                                 (replaceByIn w (Singleton w1)
                                                 e1)
                                        else
                                          Lambda w (replaceByIn v e e1)
                                        where w1 = rename w

isIn :: Expr -> Expr -> Bool
isIn (Singleton v)         (Singleton w)       = (v == w)
isIn v@(Singleton _)       (Application e1 e2) = v `isIn` e1 || v `isIn` e2 
isIn v@(Singleton c)       (Lambda c2 e)       = c == c2 || isIn v e
isIn (Application e1 e2)   e3                  = e1 `isIn` e3 || e2 `isIn` e3
isIn (Lambda v1 e1)        e2                  = (Singleton v1) `isIn` e2
                                               || e1 `isIn` e2


-- PARSER

forbidden :: Char -> Bool
forbidden v = v == ' ' 
            || v == '_' 
            || v == '(' 
            || v == ')'
            || v == '\\'
            || isNumber v

parseLambda :: String -> Either Error Expr
parseLambda [v]                = if forbidden v
                                 then Left  $ IllegalChar v 
                                 else Right $ Singleton (v, 0)
parseLambda ('(':v:")")        = parseLambda [v]
parseLambda ('(':'\\':v:'.':e) = if forbidden v 
                                 then Left $ IllegalChar v
                                 else Lambda (v, 0) <$> (parseLambda $ addParens
                                                                   $ init e)
parseLambda ('(':'\\':v:e)     = if forbidden v
                                 then Left $ IllegalChar v
                                 else Lambda (v, 0) <$> (parseLambda $ "(\\" 
                                                                     ++ e)
parseLambda ('(':s)            = Application <$> (fst splitted)
                                             <*> (snd splitted)
                  where splitted = ( parseLambda (addParens $ take (m-2) 
                                                            $ init s)
                                   , parseLambda (drop (m-1) $ init s))
                        n        = length $ takeWhile (/= 0) $ tail $ scanl f 0
                                   $ reverse $ init s
                        m        = (length $ init s) - n
                        f :: Integer -> Char -> Integer
                        f n c    = case c of
                                     '(' -> n+1
                                     ')' -> n-1
                                     otherwise -> n
parseLambda _                  = Left ParseError

addParens :: String -> String
addParens "" = ""
addParens s = if head s == '\\' || checkApps 0 s
              then '(':(s ++ ")")
              else s
  where checkApps :: Integer -> [Char] -> Bool
        checkApps 0 (' ':_)  = True
        checkApps _ []       = False
        checkApps n ('(':cs) = checkApps (n+1) cs
        checkApps n (')':cs) = checkApps (n-1) cs
        checkApps n (_:cs)   = checkApps n cs

-- TODO :
--  * Enable combinator definitions and reading them from a file.
--  ! Ensure that there are no infinite loops or evaluation errors 

-- IO

clear :: IO GHC.IO.Exception.ExitCode
clear = system "clear"

main :: IO ()
main = welcomeMsg >> repl

repl :: IO ()
repl = do hSetBuffering stdin LineBuffering
          hSetBuffering stdout NoBuffering
          putStr "λ> "
          s <- getLine
          case s of
            ":quit"   -> return ()
            ":q"      -> return ()
            ":h"      -> printHelp >> repl
            ":help"   -> printHelp >> repl
            ":clear"  -> clear >> repl
            ":cls"    -> clear >> repl
            otherwise -> case (parseLambda $ addParens s) of
                           Right l -> do putStr $ show l
                                         putStr " = "
                                         print $ evalLambda l
                                         repl
                           Left  e -> do print e >> repl

welcomeMsg :: IO ()
welcomeMsg = do putStrLn "Lambda Calculus Interpreter"
                putStrLn "Version: 0.1.2"
                putStrLn "Build Date: February 2nd, 2018"
                putStrLn "Type :help or :h for help and information on commands"

printHelp :: IO ()
printHelp = do putStrLn "Lambda Calculus Interpreter"
               putStrLn "---------------------------\n"
               putStrLn "RULES:"
               putStrLn " - Variable names are 1 character long\
                          \ and can be anything except the following:"
               putStrLn "    * Numbers\n    * Underscore (\'_\')\n    * Parens"
               putStrLn "    * Backslash (\'\\\')\n    * Space (\' \')"
               putStrLn " - Variable names can be surrounded by parenthesis:\
                          \ (v) or v"
               putStrLn " - When encoutering an ambiguous case, the interpreter\
                        \ will try to rename \n  variables by adding a number\
                        \ after them:"
               putStrLn "    * (\\xx.x) is equivalent to (\\x.(\\x.x))"
               putStrLn "     The second lambda definition shadows the first,\
                        \ so the compiler will\n     evaluate this to \
                        \(\\xx_1.x)"
               putStrLn "    * (\\xy.x y) y will evaluate to (\\y_1.y y_1)"
               putStrLn " - Functions should be preferably surrounded by\
                        \ parenthesis: "
               putStrLn "    * \\v.E will work, but.."
               putStrLn "    * \\v.E x is equivalent to \\v.(E x)"
               putStrLn "    * \\v.E x is not equivalent to (\\v.E) x"
               putStrLn " - There can be multiple parameters for a function\
                          \ definition:"
               putStrLn "    * (\\x.(\\y.x)) is correct"
               putStrLn "    * (\\xy.x) is correct and equivalent to the above"
               putStrLn " - Application is done with spaces: (E1 E2)"
               putStrLn " - There can be multiple arguments for the same\
                        \ application:"
               putStrLn "    * ((E1 v) w) is correct"
               putStrLn "    * (E1 v w) is correct and equivalent to the above"
               putStrLn "\nADDITIONAL COMMANDS:"
               putStrLn " - :quit or :q will quit the interpreter."
               putStrLn " - :help or :h will show this help message."
               putStrLn " - :cls or :clear will clear the console."
