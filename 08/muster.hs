module Musterloesung08 where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.List
import Test.QuickCheck (quickCheck)

-- Aufgabe 1
-- (c) Christiane Göhring
 
data SKVals = S | K | Le | Ri  deriving Show
 
skParser :: Parser [SKVals]
skParser = do
        oneOf "SK()"
        many $ parseString
       
symbol :: Parser Char
symbol = oneOf "SK()"

parseString :: Parser SKVals
parseString = do
        spaces
        chars
        where chars =
                (string "S" >> return S)
                <|> (string "K" >> return K)
                <|> (string "(" >> return Le)
                <|> (string ")" >> return Ri)
 
spaces :: Parser ()
spaces = skipMany space

testA1 :: IO ()
testA1 = do
    print $ parse skParser "testString" "KK(SK)"
    print $ parse skParser "testString" "S S K (S (K (S S (S (S S K)))) K)"
    print $ parse skParser "testString" "S (S K K) (S K K) (S(K(S (S K K)))(S (S K K) (S K K)))"
    print $ parse skParser "testString" "ABC"


-- Aufgabe 2
-- (c) Heiko Bächmann

class  (Monad m) => MonadPlus m  where
    mzero  :: m a
    mplus  :: m a -> m a -> m a

instance MonadPlus Maybe where
        mzero = Nothing
        Nothing `mplus` ys = ys
        xs `mplus` ys = xs
        
-- the monad laws
m = Just 3

testR1 = mzero `mplus` (Just 3)
testR2 = (Just 3) `mplus` mzero
testR3 = (Just 3) `mplus`( (Just 4) `mplus` (Just 5) )
testR3' = ( (Just 3) `mplus`(Just 4) ) `mplus` (Just 5)
testR4 = mzero >>= (\3 -> Just 3)
testR5 = ( (Just 3) >> mzero ) :: Maybe Int

testRules = (testR1 == m) && (testR2 == m)
        && (testR3 == testR3') && (testR4 == mzero)  && (testR5 == mzero) 

testA2 = testRules

-- Aufgabe 3
-- (c) Simon Pirkelmann, mit Änderungen in Teil c.

-- Teilaufgabe a)
mergeSort :: ([t] -> [t] -> [t]) -> [t] -> [t]
mergeSort _ [] = []
mergeSort _ [x] = [x]
mergeSort mixf xs =
		mixf (mergeSort mixf xs1) (mergeSort mixf xs2)
	where (xs1, xs2) = split xs

split :: [a] -> ([a],[a])
split xs = go xs xs
	where 	go (x:xs) (_:_:zs) = (x:us,vs)
			where (us, vs) = go xs zs
		go xs _ = ([],xs)

-- Teilaufgabe b)
sortfold = mergeSort mymix

mymix :: [(Char,Int)] -> [(Char,Int)] -> [(Char,Int)]
mymix xs [] = xs
mymix [] ys = ys
mymix (x:xs) (y:ys) 	| fst x < fst y = x : (mymix xs (y:ys))
			| fst x == fst y = (fst x, snd x + snd y) : (mymix xs ys)
			| fst x > fst y = y : (mymix (x:xs) ys)

-- Teilaufgabe c)
frequency s = mergeSort rankMix $ sortfold $ zip s (repeat 1)

rankMix xs [] = xs
rankMix [] ys = ys
rankMix a@(x:xs) b@(y:ys) | snd x >= snd y = x : rankMix xs b
                          | otherwise      = y : rankMix a ys
