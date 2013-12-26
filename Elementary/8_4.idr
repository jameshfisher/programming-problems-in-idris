-- Write a program that prints all prime numbers.

module Main

import Helpers

-- a List is not good as we need efficient cons and reverse access
import Data.SortedSet

sieve : List Nat -> Nat -> Bool
sieve ps n = not $ any (\p => p `divides` n) $ take_while_list (\p => p*p <= n) ps

primes_from : SortedSet Nat -> Nat -> Stream Nat
primes_from ps n =  if sieve (SortedSet.toList ps) n
                    then n :: primes_from (insert n ps) (S n)
                    else      primes_from           ps  (S n)

primes : Stream Nat
primes = primes_from empty 2

main : IO ()
main = sequence_stream $ map (putStrLn . show) primes
