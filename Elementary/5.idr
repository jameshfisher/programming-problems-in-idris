-- Modify the previous program such that only multiples of three or five are considered in the sum, e.g. 3, 5, 6, 9, 10, 12, 15 for n=17

module Main

import Helpers

fizz_buzz : Nat -> Nat
fizz_buzz n = go 0 n where
  go : Nat -> Nat -> Nat
  go acc n = case n of
    Z    => acc
    S n' => go (if (5 `divides` n) || (3 `divides` n) then acc + n else acc) n'

main : IO ()
main = do
  n <- promptNat "Provide an n: "
  putStrLn $ show $ fizz_buzz n
