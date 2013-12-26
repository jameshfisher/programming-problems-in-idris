-- Write a program that prints a multiplication table for numbers up to 12.

-- First time dependent types are useful!

module Main

import Prelude.Strings

import Helpers

Matrix : Nat -> Nat -> Type -> Type
Matrix r c a = Vect r (Vect c a)

matrix_max : Matrix r c Nat -> Nat
matrix_max = foldl max 0 . map (foldl max 0)

pad_left : Nat -> String -> String
pad_left width s = (pack $ Vect.replicate (width - length s) ' ') ++ s

show_row : Nat -> Vect n String -> String
show_row w = concat . Vect.intersperse " " . map (pad_left w)

show_table : {r:Nat} -> {c:Nat} -> Matrix r c String -> String
show_table {r} {c} m = unlines $ map (show_row (matrix_max natmat)) m
  where
    natmat : Matrix r c Nat
    natmat = map (map Strings.length) m

multiplication_table : (r:Nat) -> (c:Nat) -> Matrix r c String
multiplication_table r c = map (\r => map (show . (*r)) (one_to_n c)) (one_to_n r)

main : IO ()
main = putStrLn $ show_table $ multiplication_table 12 12
