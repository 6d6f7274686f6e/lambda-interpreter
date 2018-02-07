-- Lambda Calculus Interpreter, Version 0.1.4

import System.IO
import Data.Char

-- DATA DEFINITIONS

type Variable = (Char, Integer)

data Expr = Singleton Variable
          | Application Expr Expr
          | Lambda Variable Expr
          deriving Eq

instance Show Expr where
    show (Singleton (v, 0)) = [v]
    show (Singleton (v, i)) = [v, '_'] ++ (show $ i - 1)
--    show (Application e1@(Application _ _) e2) = (init $ show e1) ++ " " 
--                                               ++ show e2 ++ ")"
    show (Application e1 e2) = "(" ++ (show e1) ++ " " ++ (show e2) ++ ")"
--    show (Lambda v e@(Lambda _ _)) = "(λ" ++ (show (Singleton v)) 
--                                   ++ (tail $ tail $ show e)
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
  (Lambda v e)                  -> evalLambda $ replaceByIn v e2 e
evalLambda (Lambda v e)        = (Lambda v $ evalLambda e)
evalLambda (Singleton v)       = (Singleton v)

-- replace by in is Beta-equivalence
replaceByIn :: Variable -> Expr -> Expr -> Expr
replaceByIn v e (Singleton w)       = if v == w 
                                      then e
                                      else Singleton w
replaceByIn v e (Application e1 e2) = Application (replaceByIn v e e1)
                                                  (replaceByIn v e e2)
replaceByIn v e (Lambda w e1)       = if v == w || w `isFreeIn` e
                                      then replaceByIn v e
                                             (Lambda (rename w)
                                                     (replaceByIn w 
                                                       (Singleton (rename w))
                                                       e1))
                                      else Lambda w (replaceByIn v e e1)

-- Tests wether a variable is free or bound in an expression
isFreeIn :: Variable -> Expr -> Bool
isFreeIn v (Singleton w)       = v == w
isFreeIn v (Lambda w e)        = (v /= w) && v `isFreeIn` e
isFreeIn v (Application e1 e2) = (v `isFreeIn` e1) || (v `isFreeIn` e2)

-- PARSER

forbidden :: Char -> Bool
forbidden v = v `elem` "_ ()\\" || isNumber v

parseLambda :: String -> Either Error Expr
-- Single variable
parseLambda [v]                = if forbidden v
                                 then Left  $ IllegalChar v
                                 else Right $ Singleton (v, 0)
parseLambda ('(':v:")")        = parseLambda [v]
-- For compability with the show instance:
parseLambda ('(':'λ':s)       = parseLambda ('(':'\\':s)
-- Single parameter lambda expression
parseLambda ('(':'\\':v:'.':e) = if forbidden v 
                                 then Left $ IllegalChar v
                                 else Lambda (v, 0) <$> (parseLambda $ addParens
                                                                   $ init e)
-- Multiple parameters lambda expression
parseLambda ('(':'\\':v:e)     = if forbidden v
                                 then Left $ IllegalChar v
                                 else Lambda (v, 0) <$> (parseLambda $ "(\\"
                                                                     ++ e)
-- Application. There can be multiple arguments.
parseLambda ('(':s)            = Application <$> (fst splitted)
                                             <*> (snd splitted)
                  where splitted = (,) <$> parseLambda . addParens . take (m-2)
                                       <*> parseLambda . drop (m-1)
                                       $ init s
                        n        = length $ takeWhile (/= 0) $ tail $ scanl f 0
                                 $ reverse $ init s
                        m        = (length $ init s) - n
                        f n c    = case c of
                                     '(' -> n+1
                                     ')' -> n-1
                                     otherwise -> n
parseLambda _                  = Left ParseError

-- Adds parenthesis around an expression if needed
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

-- Clear the screen and go back to the top.
clear :: IO ()
clear = putStr "\x1b[2J\x1b[0;0H"

repl :: IO ()
repl = do hSetBuffering stdin LineBuffering
          hSetBuffering stdout NoBuffering
          putStr "λ> "
          s <- getLine
          case words s of
            []         -> repl
            ":quit":_  -> return ()
            ":q":_     -> return ()
            ":h":_     -> printHelp >> repl
            ":help":_  -> printHelp >> repl
            ":clear":_ -> clear >> repl
            ":cls":_   -> clear >> repl
            otherwise  -> case (parseLambda $ addParens $ unwords $ words s) of
                           Right l -> do putStr $ show l
                                         putStr " = "
                                         print $ evalLambda l
                                         repl
                           Left  e -> do print e >> repl

main :: IO ()
main = do putStrLn "Lambda Calculus Interpreter"
          putStrLn "Version: 0.1.4"
          putStrLn "Build Date: February 4nd, 2018"
          putStrLn "Type :help or :h for help and information on commands"
          repl

printHelp :: IO ()
printHelp = readFile "help.txt" >>= putStr

-- S-K-I combinators
lK = Lambda ('x', 0) (Lambda ('y', 0) (Singleton ('x', 0)))
lS = Lambda ('x', 0) (Lambda ('y', 0) (Lambda ('z', 0) 
        (Application (Application x z) (Application y z)))) 
   where x = Singleton ('x', 0)
         y = Singleton ('y', 0)
         z = Singleton ('z', 0)
lI = (Lambda ('x', 0) (Singleton ('x', 0)))

-- Transforms any lambda-term to a series of S, K and I combinators
lT :: String -> String
lT s = case parseLambda $ addParens s of
  Left err -> show err
  Right rs -> case rs of
    Singleton x       -> [fst x]
    Application e1 e2 -> '(':(lTexpr e1) ++ ' ':(lTexpr e2) ++ ")"
    Lambda x e        -> if x `isFreeIn` e 
                         then case e of 
                           Singleton x       -> "I"
                           Lambda y e        -> lT ("(\\" ++ [fst x] 
                                                   ++ '.':(lTexpr (Lambda y e))
                                                   ++ ")")
                           Application e1 e2 -> "(S " ++ lTexpr (Lambda x e1)
                                              ++ " " ++ lTexpr (Lambda x e2) 
                                              ++ ")"
                         else "(K " ++ (lTexpr e) ++ ")"
lTexpr :: Expr -> String
lTexpr = lT . show

-- Parses S-K-I series into lambda terms.
parseSKI :: String -> Either Error Expr
parseSKI s = lTinverse <$> (parseLambda $ addParens s)

lTinverse :: Expr -> Expr
lTinverse (Application e1 e2) = Application (lTinverse e1) (lTinverse e2)
lTinverse (Singleton v)       = case v of
                                 ('K', _)  -> lK
                                 ('S', _)  -> lS
                                 ('I', _)  -> lI
                                 otherwise -> (Singleton v)

evalSKI :: Expr -> Expr
evalSKI = evalLambda . lTinverse
