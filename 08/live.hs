module Live03 where

-- imports für später
import Control.Monad
import Data.Maybe
import Control.Monad.State

-- alt und klar
---------------

multiParamFu x y z = undefined

partialApplication y z = multiParamFu 42 y z
partialApplication' = multiParamFu 42
-- das passiert die ganze Zeit

-- ($) ist Applikation, (.) ist Komposition
teil1 = undefined
teil2 = undefined
teil3 = undefined

stupidParts x = teil3 (teil2 (teil1 x))
appliedParts x = teil3 $ teil2 $ teil1 x
composedParts = teil3 . teil2 . teil1  -- Achtung, kein x hier
-- composedParts heißt "point-free style". Punkte sind hier die Parameter

-- Parametrischer Polymorphismus ist allgemein
data Func a b = F (a -> b)
lam :: (a -> b) -> Func a b
lam f = F f
app :: Func a b -> a -> b
app (F f) x = f x

-- Typklassen
-------------

-- sowas wie Interfaces
data MyOwnData = D Int (Char -> Char)

class FooTypeclass a where
    foo :: String -> a

-- Typklasseninstanzen sind Spezialisierungen
instance FooTypeclass MyOwnData where
    foo = undefined

instance FooTypeclass Int where
    foo = undefined -- eine GANZ andere Definition, als oben

instance Enum MyOwnData where
    -- ....

instance Num MyOwnData where
    x + y = undefined
    -- + für Int, + für Double und + für MyOwnData machen unterschiedliches Zeug!

instance Eq MyOwnData where
    (D x _) == (D y _) = x == y

-- rekursiv, mit parametrisierten Datentypen
data OtherData a b = O a (a -> b)

instance (Num a, Num b) => Num (OtherData a b) where
    (+) = undefined
    fromInteger x = O (fromInteger x) undefined


-- Monaden
----------

-- Monade ist eine Typklasse; "higher order" -- `m a`, wir interessieren uns für `m` und nicht für `a`
-- `Maybe` ist eig. `Maybe a`

{-
instance Monad Maybe where
    Nothing >>= _ = Nothing
    (Just x) >>= f = f x -- f :: a -> Maybe b
    return = Just
    fail _ = Nothing
-- diese Definition ist in Data.Maybe schon drin
-}

firstMaybe x | x /= 0 = Just x
firstMaybe _ = Nothing

secondMaybe x | x == 42 = Just x
secondMaybe _ = Nothing

composeMaybes x = do
  first <- firstMaybe x
  second <- secondMaybe first
  return second

-- State
data Val = X | Y | Z
type StateForVal = State Val ()

newState :: StateForVal
newState = do
  put X

-- workWithState :: Int -> StateForVal
workWithState x = do
  old <- get
  -- do something MORE meaningful to old
  -- ...
  let new = newState
  -- compute new state
  put new
