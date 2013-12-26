module Matrix

Matrix : Nat -> Nat -> Type -> Type
Matrix rows cols a = Vect rows (Vect cols a)

transpose : (rows:Nat) ->
            (cols:Nat) ->
            Matrix rows cols a ->
            Matrix cols rows a

transpose Z         cols []      = replicate cols []
transpose (S rows') cols (r::rs) = zipWith (::) r (transpose rows' cols rs)
