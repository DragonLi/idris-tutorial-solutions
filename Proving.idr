module Proving

%default total

-- equality

twoPlusTwo : 2 + 2 = 4
twoPlusTwo = refl

disjoint : (n : Nat) -> Z = S n -> _|_
disjoint n p = replace {P = disjointTy} p  ()
               where
                 disjointTy : Nat -> Type
                 disjointTy Z = ()
                 disjointTy (S k) = _|_

-- simple theorems

plusReduce : (n : Nat) -> plus Z n = n
plusReduce n = refl

plusReduceZ : (n : Nat) -> n = plus n Z
plusReduceZ Z = refl
plusReduceZ (S n) = cong (plusReduceZ n)

plusReduceS : (n : Nat) -> (m : Nat) -> S (plus n m) = plus n (S m)
plusReduceS Z m = refl
plusReduceS (S n) m = cong (plusReduceS n m)

-- interactive theorem proving

plusReduceZ' : (n : Nat) -> n = plus n Z
plusReduceZ' Z = ?p_Z
plusReduceZ' (S n) = let ih = plusReduceZ' n in ?p_S


---------- Proofs ----------

Proving.p_S = proof
  intros
  rewrite ih
  trivial


Proving.p_Z = proof
  trivial

-- provisional definitions

data Parity : Nat -> Type where
  even : Parity (n + n)
  odd  : Parity (S (n + n))

parity : (n : Nat) -> Parity n
parity Z = even {n = Z}
parity (S Z) = odd {n = Z}
parity (S (S n)) with (parity n)
  parity (S (S (j + j)))     | even ?= even {n = S j}
  parity (S (S (S (j + j)))) | odd  ?= odd {n = S j}

Proving.parity_lemma_2 = proof
  intros
  rewrite sym (plusSuccRightSucc j j)
  trivial

Proving.parity_lemma_1 = proof
  intro
  intro
  rewrite sym (plusSuccRightSucc j j)
  trivial

partial

nat2Bin : Nat -> List Bool
nat2Bin Z = []
nat2Bin n with (parity n)
  nat2Bin (j + j)     | even = False :: nat2Bin j
  nat2Bin (S (j + j)) | odd  = True  :: nat2Bin j


vzipWith : (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
vzipWith f [] ys = []
vzipWith f (x :: xs) (y :: ys) = f x y :: (vzipWith f xs ys)
