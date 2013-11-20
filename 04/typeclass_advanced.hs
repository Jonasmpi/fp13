{-# OPTIONS_GHC -XMultiParamTypeClasses -XFunctionalDependencies -XFlexibleInstances -XTypeFamilies #-}

module Musterloesung4A1_advanced where

import qualified Data.List as L
-- import Data.List -- Prelude hat genug
import qualified Data.Set as S
import Data.Set (Set) -- nur Konstruktor

-- (c) Julian Neuberger / Christiane Göhring, modifiziert und erweitert
-- hier werden 'functional dependencies' verwendet, um den Compiler klar zu machen, dass der Typ b vom a abhängt
class Eq b => Collection a b | a -> b where
    insert          :: a -> b -> a
    at              :: Int -> a -> b
    filter          :: ( b -> Bool ) -> a -> a
    singleton       :: b -> a
    null            :: a -> Bool
    size            :: a -> Int
    fromList        :: [b] -> a
    -- map             :: (Eq d, Collection c d, b ~ d, a ~ c) 
    --                => (b -> d) -> a b -> c d
    fold            :: (b -> b -> b) -> a -> b

instance Eq a => Collection [a] a where 
    insert xs x                     = x : xs
    at pos xs                       = xs!!pos
    filter                          = L.filter
    singleton x                     = x:[]
    null                            = L.null
    size                            = L.length 
    fromList                        = id
    -- map                             = L.map
    -- fold                            = L.foldl1


instance Ord a => Collection (Set a) a where 
    insert                          = flip S.insert
