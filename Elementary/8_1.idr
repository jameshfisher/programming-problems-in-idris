-- Write a program that prints all prime numbers.

module Main

import Helpers

is_prime : Nat -> Bool
is_prime n = not $ any (\m => m `divides` n) $ take_while_stream (< n) $ iterate S 2

primes : Stream Nat
primes = filter_stream is_prime $ iterate S 2

main : IO ()
main = sequence_stream $ map (putStrLn . show) primes
