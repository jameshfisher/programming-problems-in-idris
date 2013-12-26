-- Write a program that asks the user for a number n and gives him the possibility to choose between computing the sum and computing the product of 1,…,n.

module Main

import Helpers

choose : String -> Maybe (Nat -> Nat)
choose s = case s of
  "t" => Just triangular
  "f" => Just fac
  _   => Nothing

%default partial

main : IO ()
main = do
  n <- promptNat "Provide an n: "
  function <- promptTil "Triangular [t] or factorial [f]?: " choose
  putStrLn $ show $ function n
