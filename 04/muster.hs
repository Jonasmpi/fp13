{-# OPTIONS_GHC -cpp #-} -- haaacks!
module Musterloesung4 where

import qualified Data.List as List
-- [] ist ja auch im Prelude drin
import qualified Data.Set as Set
import Data.Set (Set)

-- Aufgabe 1, Idee.
#if 0
class Container c where
    null :: c a -> Bool
    at :: c a -> Int -> a
    ...
    -- achten Sie auf den Kind von Container!
    -- 

instance Container [] where
    null = L.null
    at = (L.!!)
    ...

instance Container Set where
    null = S.null
    at = undefined
    ...
#endif

-- Aufgabe 1, (c) Heiko Bächmann, mit keinen Änderungen
class Container c where
        insert' :: Ord a => a -> c a -> c a
        at :: Int -> c b -> b
        filter' :: (a -> Bool) -> c a -> c a
        singleton' :: a -> c a
        null' :: c a -> Bool
        size' :: c b -> Int
        fromList' :: Ord a => [a] -> c a
        map' :: (Ord a, Ord b) => (a -> b) -> c a -> c b
        fold' :: (a -> b -> b) -> b -> c a -> b

instance Container [] where
        insert' a xs = List.insert a xs
        at a xs = (xs !! a)
        filter' = List.filter
        singleton' x = [x]
        null' = List.null
        size' = length 
        fromList' = id
        map' = List.map
        fold' = List.foldr

instance Container Set where
        insert' a xs = Set.insert a xs
        at i xs= (Set.toList xs) !! i  -- elemAt from Hoogle doesnt work
        filter' = Set.filter
        singleton' = Set.singleton
        null' = Set.null
        size' = Set.size
        fromList' = Set.fromList
        map' = Set.map
        fold' = Set.fold




-- (c) Domenic Dietel, mit Änderungen
class Concat a where
	(+++) :: a -> a -> a
	cat   :: [a] -> a
        -- die Default-Implementierungen
	(+++) a b = cat [a,b] 
        cat = foldr1 (+++)

#if 0        
instance Concat Integer where
	cat = read . concatMap show 
        -- a +++ b = cat [a,b] 
#else
instance Concat Integer where
        a +++ b = shiftAdd a b

-- warum hat keiner das so gemacht?
shiftAdd :: Integer -> Integer -> Integer
shiftAdd a b = let lenB = ceiling $ log10 b
                   log10 = logBase 10 . fromInteger 
                   shiftedA = a * 10^lenB
               in shiftedA + b
#endif

instance Concat [a] where
	(+++) = (++)
	cat = concat

#if 1
-- Aufgabe 3, (c) Heiko Bächmann
-- implemented with an minMax-tree
data Field = X | O | I deriving (Eq, Show, Read)
-- 'I' means empty
-- display is not nice, see below

type GameField = [Field]

type User = Field
type Next = Field

type Position = Int
type Eval = Int

game :: GameField
game = replicate 9 I

tryAllPositions :: Field -> GameField -> [(Int,GameField)]
tryAllPositions f gf = foldr (\ x acc ->(x, (setToPosition x f gf)):acc) [] (getEmptyFields gf)

countEmptyFields :: GameField -> Int
countEmptyFields xs = foldr (\ x acc -> if x == I then acc+1 else acc) 0 xs

getEmptyFields :: GameField -> [Int]
getEmptyFields gf = foldr (\ (i,x) acc -> if x == I then i:acc else acc) [] indices
        where indices = zip [0..] gf

setToPosition :: Int -> Field -> GameField -> GameField
setToPosition x field gf = replace x field gf
      
switchUser :: Field -> Field
switchUser f
        | f == X = O
        | otherwise = X

              
isWinner :: Field -> GameField -> Bool
isWinner field gf = threeInRow (0,1,2) || threeInRow (3,4,5) || threeInRow (6,7,8) || threeInRow (0,3,6) || threeInRow (1,4,7) || threeInRow (2,5,8) || threeInRow (0,5,8) || threeInRow (2,5,6)        
        where threeInRow (a,b,c) 
                        | gf !! a == field && 
                          gf !! b == field && 
                          gf !! c == field 
                              = True
                        | otherwise = False

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace 0 z [_] = [z]
replace 0 z (_:xs) = (z:xs)
replace i z xs = take i xs ++ z:(tail zs) 
        where (_,zs) = splitAt i xs
        
order :: (Ord a) => a -> a -> Ordering
order x y 
        | x > y = GT
        | x < y = LT
        | otherwise = EQ

        
possiblePositions :: Field -> GameField -> [(Position,Eval)]
possiblePositions _ gf
        | countEmptyFields gf == 9 = map (\ a -> (a,1)) [0..8]
possiblePositions f gf = evalAllBasic f gf

evalAllBasic :: User -> GameField -> [(Int, Int)]
evalAllBasic u gf = reverse $ List.sortBy (\ (_,a) (_,b) -> order a b) posEval
        where 
                allPos = tryAllPositions u gf
                posEval = foldr (\ (pos, g) acc -> (pos, evalAll u (switchUser u) g):acc) [] allPos


evalAll :: User -> Next -> GameField -> Int
evalAll u n gf 
        | ev /= 0 = ev
        | countEmptyFields gf == 0 = 0
        | otherwise = foldr (\ (_,g) acc -> acc + evalAll u (switchUser n) g ) 0 allPos
        where 
                ev = eval u gf
                allPos = tryAllPositions n gf
                

eval :: User -> GameField -> Int
eval f gf 
        | isWinner f gf = 1
        | isWinner (switchUser f) gf  = -1
        | otherwise = 0

test33,test35,test37,test38, test39 :: String
test33 = show ( possiblePositions X testField)
test39 = show ( possiblePositions X testField2)
test35 = show ( possiblePositions X testField3)
test37 = show ( possiblePositions X testField4)
test38 = show ( possiblePositions O testField5)

testField,testField2,testField3, testField4,testField5 :: GameField
testField = [X,I,O,I,X,I,O,O,X]
testField2 = replicate 9 I
testField3 = [X,I,O,I,X,I,O,I,I]
testField4 = [X,I,I,I,I,I,O,I,I]
testField5 = [X,I,I,I,I,I,I,I,I]

#else
-- Aufgabe 3, wozu haben wir Monaden gelernt?
-- (c) Julian Neuberger / Christiane Göhring, mit keinen Änderungen
-- nicht so vollständig, wie oben, aber mit Paar netten Tricks
type Row = (Cell, Cell, Cell)
type Board = (Row, Row, Row)

data Cell = X | O | B
    deriving (Eq, Ord)
		
instance Show Cell where
	show x 
		| x == X = "X"
		| x == O = "O"
		| otherwise = " "
-- und schon haben wir eine nette Ausgabe
	
testBoard :: Board
testBoard = ((B, B, B), (X, O, B), (B, B, B))

-- monaden!
replaceFirst :: Monad m => (a -> m d) -> (a,b,c) -> m (d,b,c)
replaceFirst func (a,b,c) = func a >>= \d -> return (d,b,c)

replaceSecond :: Monad m => (b -> m d) -> (a,b,c) -> m (a,d,c)
replaceSecond func (a,b,c) = func b >>= \d -> return (a,d,c)

replaceThird :: Monad m => (c -> m d) -> (a,b,c) -> m (a,b,d)
replaceThird func (a,b,c) = func c >>= \d -> return (a,b,d)

possibleMoves' p board = replaceFirst  rowMoves   board ++
                         replaceSecond rowMoves   board ++
                         replaceThird  rowMoves   board
    where rowMoves row = replaceFirst  fieldMoves row ++
                         replaceSecond fieldMoves row ++
                         replaceThird  fieldMoves row
          fieldMoves B = [p]
          fieldMoves _ = []
-- kein Ranking, dafür aber recht elegante Auflistung. (++) könnte man noch verbessern.
#endif

-- Aufgabe 4, a la Heiko Bächmann
count :: Char -> String -> Int
count c str = let helper acc str1 | str1 == c = acc + 1 
                                  | otherwise = acc
              in foldl helper 0 str

-- (c) Sandra Greiner, mit keinen Änderungen
count2::Char -> String -> Int
count2 x ys = length $ filter (x==) ys

testList1, testList2 :: [Int]
testList1 = [1..5]
testList2 = [10..14]

testZipWith' :: String
testZipWith' = show testList1 ++ " plus " ++ show testList2 ++ " is: " ++  show (zipWith' (+) testList1 testList2)


-- Aufgabe 5, a la Heiko Bächmann
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = map  (uncurry f) $ zip xs ys

zipWith'' f xs = map (uncurry f) . zip xs
