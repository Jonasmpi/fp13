Die natürlichen Zahlen, a la "Church numerals".

\begin{code}
module Nat where
\end{code}

\begin{code}
data Nat = Zero | Succ Nat
           deriving Show
\end{code}

\begin{code}
foldN :: a -> (a -> a) -> Nat -> a
foldN z s Zero     = z
foldN z s (Succ n) = s (foldN z s n)
\end{code}

Zu bearbeiten:
\begin{code}
addN, multN, powerN :: Nat -> Nat -> Nat
addN = undefined
multN = undefined
powerN = undefined
\end{code}

Ein Beispiel: successor definiert (+1).
\begin{code}
successor :: Nat -> Nat
successor = Succ
\end{code}

Eine ganz andere Frage: Kann man Nat auch dekrementieren, also (-1) definieren? Wie macht man das "sicher"?
