-- Write a program that asks the user for a number n and prints the sum of the numbers 1 to n

module Main

import Helpers

%default partial

main : IO ()
main = do
  n <- promptNat "Provide an n: "
  putStrLn $ show $ triangular n
