{-# OPTIONS -Wall #-}

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort left) (mergesort right)
  where
    n = length xs `div` 2
    (left, right) = splitAt n xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y 
                        then x : merge xs (y:ys) 
                        else y : merge (x:xs) ys 
