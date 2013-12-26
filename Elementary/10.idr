-- Write a program that prints the next 20 leap years.

module Main

import Helpers

is_leap : Nat -> Bool
is_leap y = (400 `divides` y) || ((not (100 `divides` y)) && (4 `divides` y))

main : IO ()
main = sequence_ $ map (putStrLn . show) $ Stream.take 20 $ filter_stream is_leap $ nats_from 2013
