module Main where

main :: IO ()
main = do
  putStrLn "hello world"




divisorHelper n i | n < i = 0
                  | n `mod` i == 0  =  1 + divisorHelper n (i + 1)
                  | otherwise = divisorHelper n (i + 1)


divisor 1 = 1
divisor n = divisorHelper n 1