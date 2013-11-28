import Control.Monad.State

data Bio = B Name Age
           deriving Show
type Name = String
type Age = Int

-- State hat Zustand und "Score", genauergesagt: Zustand und den üblichen "inneren Typ"
type StateBio = State Bio ()

initBio :: StateBio
initBio = put $ B "Fritzi" 0

birthday :: StateBio
birthday = do
  b@(B name age) <- get
  put $ B name (age+1)

rename :: String -> StateBio
rename newName = do
  b@(B name age) <- get
  let newB = B newName age
  put newB

undercover :: StateBio
undercover = do
  b@(B name age) <- get
  put $ B "Agent Smith" age
  return ()

action = do
           initBio
           birthday
           birthday
           rename "Fritz"
           birthday
           undercover
           birthday

test = execState action $ B "Mäxchen" 1
