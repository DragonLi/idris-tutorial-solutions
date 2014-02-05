module dependenttypes

-- vectors

data Vec : Nat -> Type -> Type where
  Nil  : Vec Z a
  (::) : a -> Vec k a -> Vec (S k) a

(++) : Vec n a -> Vec m a -> Vec (n + m) a
(++) Nil ys       = ys
(++) (x :: xs) ys = x :: xs ++ ys
