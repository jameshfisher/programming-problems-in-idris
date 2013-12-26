module Main where

flip_diag :: [[a]] -> [[a]]
flip_diag t = case t of
  []   -> []
  [r]  -> map (:[]) r
  r:rs -> zipWith (:) r (flip_diag rs)
