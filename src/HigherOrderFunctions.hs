import Data.Char
import Data.List

roundList = map round

lowerString = map toLower


removePunctuation [] = [] 
removePunctuation (x:xs) 
  | not (isPunctuation x) = x : removePunctuation xs
  | otherwise = removePunctuation xs


removePunctuationUsingFilter = filter (not . isPunctuation)
 
clean = filter(\x -> isLetter x || x == ' ') . map toLower



wordFrequencies s =
   let cleaned = clean s
       groupedWords = (group . sort . words) cleaned
   in map (\xs -> (head xs, length xs)) groupedWords


-- function maps over first item in tuple (hello, 5) 
-- uses filter on the second item of the tuple 5 and returns it if greater than 5
frequentWordsOnly = map fst .filter(\x -> snd x >= 5)

--frequentWordsOnly [(hello, 7), (world, 3)]