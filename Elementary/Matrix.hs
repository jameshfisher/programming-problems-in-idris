module Matrix where

transpose :: [[a]] -> [[a]]
transpose t = case t of
  r:[] -> map (:[]) r
  r:rs -> zipWith (:) r (transpose rs)
