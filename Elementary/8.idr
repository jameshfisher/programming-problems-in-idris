-- Write a program that prints all prime numbers.

module Main

import Helpers

is_prime : Nat -> Bool
is_prime n = not $ any (\m => m `divides` n) $ take_while (\m => m*m <= n) $ iterate S 2

primes : Stream Nat
primes = filter is_prime nats

main : IO ()
main = sequence_stream $ map (putStrLn . show) primes
