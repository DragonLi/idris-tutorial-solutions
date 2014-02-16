module WellTypedInterp

-- types

data Ty = TyInt | TyBool | TyFun Ty Ty

-- type interpretation

interpTy : Ty -> Type
interpTy TyInt = Int
interpTy TyBool = Bool
interpTy (TyFun l r) = (interpTy l) -> (interpTy r)

-- G will be an implicit argument used frequently

using (G : Vect n Ty)

  data HasType : (i : Fin n) -> Vect n Ty -> Ty -> Type where
    Stop : HasType fZ (t :: G) t
    Pop  : HasType k G t -> HasType (fS k) (u :: G) t

  data Expr : Vect n Ty -> Ty -> Type where
    Var : HasType i G t -> Expr G t
    Val : (x : Int) -> Expr G TyInt
    Lam : Expr (a :: G) t -> Expr G (TyFun a t)
    App : Expr G (TyFun a t) -> Expr G a -> Expr G t
    Op  : (interpTy a -> interpTy b -> interpTy c) -> Expr G a -> Expr G b -> Expr G c
    If  : Expr G TyBool -> Expr G a -> Expr G a -> Expr G a

  data Env : Vect n Ty -> Type where
    Nil  : Env Nil
    (::) : interpTy a -> Env G -> Env (a :: G)

  lookup : HasType i G t -> Env G -> interpTy t
  lookup Stop   (x :: xs) = x
  lookup (Pop i)(x :: xs) = lookup i xs

  interp : Env G -> Expr G t -> interpTy t
  interp env (Var v) = lookup v env
  interp env (Val n) = n
  interp env (Lam e) = \x => interp (x :: env) e
  interp env (App l r) = (interp env l) (interp env r)
  interp env (Op o e e') = o (interp env e) (interp env e')
  interp env (If e e' e'') = if interp env e then interp env e' 
                                             else interp env e''

  add : Expr G (TyFun TyInt (TyFun TyInt TyInt))
  add = Lam (Lam (Op (+) (Var Stop) (Var (Pop Stop))))

  -- bar '|' indicates that the first parameter will be evaluated lazily

  app : |(f : Expr G (TyFun a t)) -> Expr G a -> Expr G t
  app = \f, a => App f a

  -- factorial

  fact : Expr G (TyFun TyInt TyInt)
  fact = Lam (If (Op (==) (Var Stop) (Val 0))
             (Val 1) (Op (*) (app fact (Op (-) (Var Stop) (Val 1)))
             (Var Stop)))

  -- eval

  class Cast a b where
    cast : a -> b
  
--blah : IO ()
--blah = do putStr "Enter a number:"
--          x <- getLine
--          print (interp [] (cast x))
