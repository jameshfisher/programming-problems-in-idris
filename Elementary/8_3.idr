-- Write a program that prints all prime numbers.

module Main

import Helpers

sieve : List Nat -> Nat -> Bool
sieve l n = not $ any (\p => p `divides` n) l

primes_from : List Nat -> Nat -> Stream Nat
primes_from ps n =  if sieve ps n
                    then n :: primes_from (n::ps) (S n)
                    else      primes_from     ps  (S n)

primes : Stream Nat
primes = primes_from [] 2

main : IO ()
main = sequence_stream $ map (putStrLn . show) primes
