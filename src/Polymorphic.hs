
containsMatching test [] = False


containsMatching test (x:xs)
  | test x == True = True
  | otherwise = containsMatching test xs


polyLength :: [a] -> Int
polyLength [] = 0

polyLength (x:xs) = 1 + polyLength xs


--The result of applying . is a function from a to c. On an argument of type x :: a, we first apply the function g :: a -> b, yielding g x :: b. Next, we apply f :: b -> c to g x, yielding the result of type c.

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)