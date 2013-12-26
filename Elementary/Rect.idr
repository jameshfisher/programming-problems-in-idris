Rect : Nat -> Nat -> Type -> Type
Rect width height a = Vect width (Vect height a)

flipRow : (rows:Nat) -> Vect (S rows) a -> Rect (S rows) 1 a
flipRow _ r = map (::[]) r

flip : (cols:Nat) -> (rows:Nat) -> Rect (S cols) (S rows) a -> Rect (S rows) (S cols) a
flip cols rows t = case (cols, t) of
  (Z,     r::[]) => flipRow (S rows) r
  (cols', r::rs) => zipWith (::) r (flip cols' rows rs)
