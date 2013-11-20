-- Ein Spiel mit State Monade
--
-- Eingabe: Zeichenkette aus 'a', 'b' und 'c'
-- 'a' == +1, 'b' == -1, aber nur falls Spiel “an”
-- 'c' == Spiel an/aus
-- Also: "ab" == 0, "caa" == 2
-- Man muss "merken" ob 'c' bereits da war
--
-- Quelle: http://www.haskell.org/haskellwiki/State_Monad
--
module StateGame where 
import Control.Monad.State

type GameValue = Int
type GameState = (Bool, Int)

playGame :: String -> State GameState GameValue
playGame []     = do
    (_, score) <- get
    return score
playGame (x:xs) = do
    (on, score) <- get
    case x of
         'a' | on -> put (on, score + 1)
         'b' | on -> put (on, score - 1)
         'c'      -> put (not on, score)
         _        -> put (on, score)
    playGame xs

startState = (False, 0)
 
main = print $ evalState 
  (playGame "abcaaacbbcabbab") startState
