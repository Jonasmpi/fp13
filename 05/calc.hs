module Main where

data Expr = Con Int | Bin Op Expr Expr 
            deriving Show
data Op   = Add | Sub | Mul | Div 
            deriving (Show, Eq)


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
evalM (Con a)      = return a
evalM (Bin op a b) = evalM a >>= \ n ->
                     evalM b >>= \ m ->
                     return (sem op n m)

-- Identitätsmonade
newtype Id a = Id a deriving Show

instance Monad Id where
  return  a =  Id a
  (Id a) >>= f = f a

runId (Id x) = x


-- Erweiterung um Fehlerbehandlung
data Result a  = Raise Exception | Return a   
                 deriving Show
type Exception = String

-- Fehlerbehandlungsmonade
instance Monad Result where
  return  a  = Return a
  a >>= f    = case a of
                 Raise e  -> Raise e
                 Return n -> f n

-- .... und entsprechend erweiterter Auswerter
evalFM              :: Expr -> Result Int
evalFM (Con a)      = return a
evalFM (Bin op a b) = evalFM a >>= \ n ->
                      evalFM b >>= \ m ->
                      if m == 0 && op == Div
                        then Raise "Division durch Null"
                        else Return (sem op n m)
-- das geht auch schöner mit do-Notation

runResult :: Result a -> a
runResult (Return x) = x
runResult (Raise e) = error e

test_expr = Bin Add (Con 2) (Bin Mul (Con 2) (Con 3))

test =   (==) 8 $             eval   $ test_expr
testM =  (==) 8 $ runId $     evalM  $ test_expr 
testFM = (==) 8 $ runResult $ evalFM $ test_expr
