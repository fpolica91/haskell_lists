module Main where

main :: IO ()
main = do
  putStrLn "hello world"




divisorHelper n i | n < i = 0
                  | n `mod` i == 0  =  1 + divisorHelper n (i + 1)
                  | otherwise = divisorHelper n (i + 1)


divisor 1 = 1
divisor n = divisorHelper n 1

empty = []
ints = [1,2,3]
functions = [(+1), (*2)]
nested = [["hello"], ["how are you"]]
-- If x is an element of type a, and xs is a list of type [a], then l = x : xs
containsMatchingInt :: (Int -> Bool) -> [Int] -> Bool
containsMatchingInt _ [] = False


-- test will be a function or operator
-- example of how to run containsMatchingInt (>5) [2, -3, 7]
-- another example containsMatchingInt (\x -> x `mod` 3 == 0) [-2, 4, 8)
-- x is the first item of the list 
-- xs refers to the remainder values
containsMatchingInt test (x:xs)
 | test x == True = True
 | otherwise = containsMatchingInt test xs


findInList _ [] =  error "empty list or number no in list"

findInList number (x:xs)
  | number == x = x
  | otherwise =  findInList number xs                             