module foo

--name spaces

namespace x
  test : Int -> Int
  test x = x * 2

namespace y
  test : Int -> Int
  test x = x + 2

-- parameters

parameters (x : Nat, y : Nat)
  addAll : Nat -> Nat
  addAll z = x + y + z

