import Data.Char

roundList = map round

lowerString = map toLower


removePunctuation [] = [] 
removePunctuation (x:xs) 
  | not (isPunctuation x) = x : removePunctuation xs
  | otherwise = removePunctuation xs


removePunctuationUsingFilter = filter (not . isPunctuation)