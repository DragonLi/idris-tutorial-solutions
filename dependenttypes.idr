module dependenttypes

-- vectors

data Vec : Nat -> Type -> Type where
  Nil  : Vec Z a
  (::) : a -> Vec k a -> Vec (S k) a

(++) : Vec n a -> Vec m a -> Vec (n + m) a
(++) Nil ys       = ys
(++) (x :: xs) ys = x :: xs ++ ys

-- finite sets

data MyFin : Nat -> Type where
  fZ : MyFin (S k)
  fS : MyFin k -> MyFin (S k)

-- note that in Idris, implicit arguments are inferred, so I do not
-- need to give index the following type
-- index : {n : Nat} -> {a : Type} -> Fin n -> Vec n a -> a

index : Fin n -> Vec n a -> a
index fZ     (x :: xs) = x
index (fS n) (x :: xs) = index n xs

-- membership predicate

data In : a -> Vec n a -> Type where
  here  : {x : a} -> {xs : Vec n a} -> In x (x :: xs)
  there : {x : a} -> {y : a} -> {xs : Vec n a} -> In x xs -> In x (y :: xs)

testVec : Vec 4 Int
testVec = 3 :: 4 :: 5 :: 6 :: Nil

inVec : In 5 testVec
inVec = there (there here)

-- another definition of In, with a using clause

using (x : a, y : a, xs : Vec n a)
  data In' : a -> Vec n a -> Type where
    here'  : In' x (x :: xs)
    there' : In' x xs -> In' x (y :: xs)

-- mutual definitions

mutual
  even' : Nat -> Bool
  even' Z = True
  even' (S n) = odd' n

  odd' : Nat -> Bool
  odd' Z = False
  odd' (S n) = even' n
