module typeclasses

-- type classes in Idris

class F a b where
  f : a -> b

class G a where
  g : a

instance F Bool Bool where
  f True = True
instance G Bool where
  g = True

--instance Show Nat where
--  show Z = "Z"
--  show (S k) = "S " ++ show k
