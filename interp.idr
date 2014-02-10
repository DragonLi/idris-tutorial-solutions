module interp

data Expr = Var String | Val Int | Add Expr Expr

data Ev : Type -> Type where
  MkEval : (List (String, Int) -> Maybe a) -> Ev a

fetch : String -> Ev Int
fetch x = MkEval (\ e => fetchVal e) where
          fetchVal : List (String , Int) -> Maybe Int
          fetchVal [] = Nothing
          fetchVal ((v, val) :: vs) = if x == v then Just val else fetchVal vs

--weird error. I'll ask on free node about this.

instance Functor Ev where
  fmap f (MkEval g) = MkEval (\e => fmap f (g e))
