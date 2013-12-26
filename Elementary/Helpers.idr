module Problems.Helpers

readNat : String -> Maybe Nat
readNat s = go 0 (unpack s) where

  readDigit : Char -> Maybe Nat
  readDigit c = case c of
    '0' => Just 0
    '1' => Just 1
    '2' => Just 2
    '3' => Just 3
    '4' => Just 4
    '5' => Just 5
    '6' => Just 6
    '7' => Just 7
    '8' => Just 8
    '9' => Just 9
    _   => Nothing

  go : Nat -> List Char -> Maybe Nat
  go acc l = case l of
    [] => Just acc
    (c::cs) => do
                  d <- readDigit c
                  go ((acc * 10) + d) cs

infixl 1 >>>

(>>>) : Monad m => m a -> m b -> m b
m >>> k = m >>= (\_ => k)

%default partial
getLineWithoutNewline : IO String
getLineWithoutNewline = map (fst . Prelude.Strings.break (== '\n')) getLine

prompt : String -> IO String
prompt p = putStr p >>> getLineWithoutNewline

promptTil : {a:Type} -> String -> (String -> Maybe a) -> IO a
promptTil p f = do
  try <- prompt p
  case f try of
    Nothing => promptTil p f
    Just x  => return x

promptNat : String -> IO Nat
promptNat p = promptTil p readNat
%default total

divides : Nat -> Nat -> Bool
divides n m = (m `modNat` n) == Z

fold_nat : (Nat -> Nat -> Nat) -> Nat -> Nat -> Nat
fold_nat f start n = go start n where
  go : Nat -> Nat -> Nat
  go acc n = case n of
    Z    => acc
    S n' => go (f acc n) n'

triangular : Nat -> Nat
triangular = fold_nat (+) 0

fac : Nat -> Nat
fac = fold_nat (*) 1

-- from benchmarks/quasigroups/Solver.idr; should be in Prelude.Strings
unlines : Vect n String -> String
unlines Nil = ""
unlines (l::Nil) = l
unlines (l::ls) = pack (foldl addLine (unpack l) (map unpack ls))
  where
    addLine : List Char -> List Char -> List Char
    addLine w s = w ++ ('\n' :: s)

snoc : Vect n a -> a -> Vect (S n) a
snoc []      y = [y]
snoc (x::xs) y = x::(snoc xs y)

-- [n, n+1, ..., n+k]
nats_from_to : (n:Nat) -> (k:Nat) -> Vect k Nat
nats_from_to n  Z    = []
nats_from_to n (S k) = n :: nats_from_to (S n) k

-- [n, n+1, n+2, ..., m-1]
n_to_m : (n:Nat) -> (m:Nat) -> Vect (m-n) Nat
n_to_m n m = nats_from_to n (m-n)

-- [1, 2, 3, ..., n]
--one_to_n : (n:Nat) -> Vect n Nat
--one_to_n n = n_to_m (S Z) (S n)

nats_from : Nat -> Stream Nat
nats_from = iterate S

nats : Stream Nat
nats = nats_from Z

take_while_list : (a -> Bool) -> List a -> List a
take_while_list p [] = []
take_while_list p (x::xs) =   if p x
                              then x :: take_while_list p xs
                              else []

%default partial
take_while_stream : (a -> Bool) -> Stream a -> List a
take_while_stream p (x::xs) =   if p x
                                then x :: take_while_stream p xs
                                else []

filter_stream : (a -> Bool) -> Stream a -> Stream a
filter_stream p (x::xs) =  if p x
                    then x :: filter_stream p xs
                    else filter_stream p xs

sequence_stream : Stream (IO a) -> IO ()
sequence_stream (a::as) = a >>> sequence_stream as
%default total