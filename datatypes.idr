module datatypes

-- simple definition of data types and functions over them

data MyNat = Zero | Succ MyNat

data MyList a = Nil | (:>) a (MyList a)

infixr 10 :>

-- addition over my nat

plusMyNat : MyNat -> MyNat -> MyNat
plusMyNat Zero m = m
plusMyNat (Succ n) m = Succ (plusMyNat n m)


reverseMyList : MyList a -> MyList a
reverseMyList xs = revAcc Nil xs where
                     revAcc : MyList a -> MyList a -> MyList a
                     revAcc acc Nil = acc
                     revAcc acc (x :> xs) = revAcc (x :> acc) xs

-- in idris, where blocks can have local data type declarations!

foo : Int -> Int
foo x = case isLT of
           Yes => x * 2
           No  => x * 4
        where
          data MyLT = Yes | No

          isLT : MyLT
          isLT = if x < 20 then Yes else No

-- in idris, we can omit some type annotations.

even : Nat -> Bool
even Z = True
even (S n) = odd n where
  odd Z = False
  odd (S n) = even n

test : List Nat
test = [c (S 1), c Z, d (S Z)]
       where
         c x = 42 + x
         d y = c (y + 1 + z y)
               where
                 z w = y + w
