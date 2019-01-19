{-# LANGUAGE LambdaCase #-}

import Data.Char
import qualified Data.Map as M
import System.IO
import Control.Monad
import Control.Applicative (some)
import Text.ParserCombinators.Parsec

data Expr = App Expr Expr
          | Singleton Var
          | Lambda Var Expr
          deriving (Eq)

instance Show Expr where
    show (Singleton (v, 0))        = [v]
    show (Singleton (v, i))        = v:"_" ++ show i
    show (App e1@(App _ _) e2)     = (init $ show e1) ++ " " ++ show e2 ++ ")"
    show (App e1 e2)               = "(" ++ (show e1) ++ " " ++ (show e2) ++ ")"
    show (Lambda v e@(Lambda _ _)) = "(λ" ++ (show (Singleton v)) ++ (tail $ tail $ show e)
    show (Lambda v e)              = "(λ" ++ (show (Singleton v)) ++ "." ++ show e ++ ")"

type Var = (Char, Int)
type Env = M.Map Var Expr

eval :: Expr -> Env -> Expr
eval (Singleton v) = maybe (Singleton v) id . M.lookup v
eval (Lambda v e)  = rename v e >>= \x -> Lambda x . eval e . M.insert v (Singleton x)
eval (App e1 e2)   = eval e1 >>= \case
  Lambda v e -> if v `isFreeIn` e2
                then let w = (+1) <$> v 
                     in (eval =<< eval e . M.insert v (Singleton w))
                      . (M.insert w =<< eval e2)
                else eval e . (M.insert v =<< eval e2)
  x          -> App x . eval e2

-- renames a Variable in Expr, only if necessary
rename :: Var -> Expr -> Env -> Var
rename v e env = if M.null $ M.filterWithKey (\w f -> v `isFreeIn` f && w `isFreeIn` e) env
                 then v
                 else (+1) <$> v

isFreeIn :: Var -> Expr -> Bool
isFreeIn v (Singleton w) = v == w
isFreeIn v (Lambda w e)  = (v /= w) && v `isFreeIn` e
isFreeIn v (App e1 e2)   = (v `isFreeIn` e1) || (v `isFreeIn` e2)

parseExpr :: Parser Expr
parseExpr =   try (foldl App <$> factor <*> some (char ' ' *> factor))
          <|> factor
  where factor =   try (do char '\\' <|> char 'λ'
                           vars <- some var
                           char '.'
                           flip (foldr Lambda) vars <$> parseExpr)
               <|> try (Singleton <$> var)
               <|> (char '(' *> parseExpr <* char ')')
        var    =   try ((,) <$> letter <*> (read <$> (char '_' >> some digit)))
               <|> (flip (,) 0) <$> letter

parseLambda :: String -> Either ParseError Expr
parseLambda = parse (parseExpr <* eof) ""

-- SKI combinators
envSKI :: Env
envSKI = M.fromList [ (('S', 0), lS)
                    , (('K', 0), lK)
                    , (('I', 0), lI)]
  where lK = Lambda x (Lambda y (Singleton x))
        lS = Lambda x (Lambda y (Lambda z (App (App (Singleton x) (Singleton z))
                                               (App (Singleton y) (Singleton z)))))
        x = ('x', 0)
        y = ('y', 0)
        z = ('z', 0)
        lI = Lambda x (Singleton x)

-- transforms any expression to an equivalent using only SKI combinators and
-- free variable names
transform :: Expr -> Expr
transform (Singleton x) = Singleton x
transform (App e1 e2)   = App (transform e1) (transform e2)
transform (Lambda v e)
  | v `isFreeIn` e = case e of
                       Singleton x -> Singleton ('I', 0)
                       App e1 e2   -> App (App (Singleton ('S', 0))
                                               (transform (Lambda v e1)))
                                          (transform (Lambda v e2))
                       Lambda w e2 -> transform (Lambda v (transform (Lambda w e2)))
  | otherwise      = App (Singleton ('K', 0)) (transform e)

repl :: Env -> IO ()
repl env = do hSetBuffering stdin LineBuffering
              hSetBuffering stdout NoBuffering
              putStr "λ> "
              s <- getLine
              case words s of
                []            -> repl env
                ":quit":_     -> return ()
                ":q":_        -> return ()
                ":help":_     -> printHelp >> repl env
                ":h":_        -> printHelp >> repl env
                ":clear":_    -> clear >> repl env
                ":cls":_      -> clear >> repl env
                ":set":[c]:ss -> case parseLambda $ unwords ss of
                                   Right l -> repl $ M.insert (c, 0) l env
                                   Left e  -> print e >> repl env
                ":SKI":ss     -> process eval (M.union envSKI env) ss
                ":T":ss       -> process (const . transform) env ss
                ss            -> process eval env ss
            where process f env s = case (parseLambda $ unwords s) of
                                      Right l -> do putStr $ show l
                                                    putStr " = "
                                                    print $ f l env
                                                    repl env
                                      Left  e -> print e >> repl env

main :: IO ()
main = do putStrLn "Lambda Calculus Interpreter"
          putStrLn "Version: 0.6.0"
          putStrLn "Build Date: January 17th, 2019"
          putStrLn "Type :h or :help for help."
          repl M.empty

clear :: IO ()
clear = putStr "\x1b[2J\x1b[0;0H"

-- TODO:
--  * Enable reading combinator definitions from a file.
--  * Add a preventive system for infinite loops.

printHelp :: IO ()
printHelp = readFile "help.txt" >>= putStr
