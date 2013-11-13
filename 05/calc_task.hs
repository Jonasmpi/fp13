module Calc where

data Expr = Con Int | Bin Op Expr Expr 
            deriving Show
data Op   = Add | Sub | Mul | Div 
            deriving (Show, Eq)


-- Auswerter 
eval ::  Expr -> Int
eval (Con a)      = a
eval (Bin op a b) = sem op (eval a) (eval b)

sem :: Op -> (Int -> Int -> Int)
sem Add = (+)
sem Sub = (-)
sem Mul = (*)
sem Div = div

-- Monadischer Auswerter 
evalM              :: Expr -> Id Int
evalM = undefined

-- Identitätsmonade
newtype Id a = Id a deriving Show

runId = undefined


-- Erweiterung um Fehlerbehandlung
data Result a  = Raise Exception | Return a   
                 deriving Show
type Exception = String

-- Fehlerbehandlungsmonade
instance Monad Result where
  return  a  = undefined
  a >>= f    = undefined

runResult :: Result a -> a
runResult = undefined

-- .... und entsprechend erweiterter Auswerter
evalFM              :: Expr -> Result Int
evalFM = undefined

test_expr = Bin Add (Con 2) (Bin Mul (Con 2) (Con 3))

test =   (==) 8 $             eval   $ test_expr
testM =  (==) 8 $ runId $     evalM  $ test_expr 
testFM = (==) 8 $ runResult $ evalFM $ test_expr
