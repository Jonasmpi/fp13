module SimpleState where

import Control.Monad

type SimpleState s a = s -> (a, s)
returnSt a s = (a, s)

bindSt :: SimpleState s a 
       -> (a -> SimpleState s b)
       -> SimpleState s b
bindSt step makeStep oldState
  = let (result, newState) = step oldState 
        -- run step with our initial state oldState.
    in (makeStep result) newState
    -- denkt dran: step ist eine Funktion!

getSt :: SimpleState s s
getSt = \s -> (s, s)

putSt :: s -> SimpleState s ()
putSt s = \_ -> ((), s)

{-
-- das funktioniert so nicht, gibt aber die Idee wieder
instance Monad (SimpleState s) where
  (>>=) = bindSt
  return = returnSt
-}

