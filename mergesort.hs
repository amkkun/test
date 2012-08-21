{-# OPTIONS -Wall #-}

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge left right
  where
    n = length xs `div` 2
    left = mergesort $ take n xs
    right = mergesort $ drop n xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys 
