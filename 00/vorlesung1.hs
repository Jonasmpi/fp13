-- das ist ein Kommentar
module Vorlesung1 where

-- konstante Funktion, keine Typannotation
foo = 42

-- Typdeklaration:
-- Tic-Tac-Toe, Werte für X, O und leere Zelle
-- Val ist ein Typ
-- X, O, etc. sind Konstruktoren
data Val = X 
         | O
         | Empty

-- Konstruktoren mit Parametern
data FooX = FooX Int String
-- Konstruktoren müssen nicht mitm Typ übereinstimmen
data Foo = F Int String

-- Verwenden von Konstruktoren
-- makeFoo erstellt einen neuen Wert von Typ Foo
makeFoo :: Int -> String -> Foo 
makeFoo x string = F x ("haha " ++ string)

-- Wert von Typ Val, eig.: eine Fu'n mit 0 Parameter
aValue :: Val
aValue = X

aFloat :: Double
aFloat = 42.3

-- Double nach Double nach Double heißt:
-- Double und Double bekommen, letzten Double liefern
addF :: Double -> Double -> Double
addF x y = x+y

-- selber Typ, andere Funktion
multF :: Double -> Double -> Double
multF x y = x*y

-- Zusammensetzung von Funktionen, insbez. Klammerung
bar x y z = multF z (addF x y)

-- Das selbe in C
-- // hello C!
-- multF(z, addF(x,y))

-- be a Maybe?
-- Definition aus der Vorlesung:
-- data Maybe a = Nothing | Just a
-- merke: Maybe in polymorph
foo :: Maybe a -> Int
foo Nothing = 42
foo (Just x) = makeInt x
-- makeInt macht irgendwas aus Typ a

-- Pattern Matching
-- Beachte: konkrete Werte zuerst
defaultMaybe :: Maybe Int -> Int
defaultMaybe Nothing  = 42
defaultMaybe (Just x) = x
-- jetzt falsch: defaultMaybe Nothing = booo!
-- jetzt falsch: defaultMaybe x = x^2

-- letzte Gleichung: ein beliebiges x
-- vgl. auch "catch all" aus Vorlesung
notsodefaultMaybe :: Maybe Int -> Int
notsodefaultMaybe Nothing  = 42
notsodefaultMaybe x        = 96^2
