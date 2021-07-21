-- folds are functions that operate on lists and return a single value

append [] ys = ys


append(x:xs) ys = x:(append xs ys)

takeAsLong [] _ = []

takeAsLong (x:xs) fn 
  | fn x  = x : takeAsLong xs fn
  | otherwise = []


findInList :: (Eq a) => a -> [a] -> a



findInList match (x:xs) 
  | match == x = x
  | match /= x = findInList match xs
  |otherwise = error "item not found in list"


naiveReverse :: [a] -> [a]
naiveReverse [] = []
naiveReverse (x:xs) = reverse xs ++ [x]


fastReverseAcc acc [] = acc
fastReverseAcc acc (x:xs) = fastReverseAcc (x:acc) xs


fastReverse xs = fastReverseAcc [] xs


fastReverseUsingWhere xs = fastReverseAcc [] xs
    where fastReverseAcc acc []       = acc
          fastReverseAcc acc (x : xs) = fastReverseAcc (x:acc) xs