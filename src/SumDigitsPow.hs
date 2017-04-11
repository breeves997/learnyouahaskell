module Codewars.Sumdigpow where

sumDigPow :: Int -> Int -> [Int]
sumDigPow a b = 
  let d = map digs [a, (a+1)..b]
  --in filter (\x -> sumPow x 1 == fromDigits x) d
  in [fromDigits x | x <- d, sumPow x 1 == fromDigits x]

digs :: Int -> [Int]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d
   
sumPow :: [Int] -> Int -> Int
sumPow [] x = 0
sumPow (x : xs) y = x^y + sumPow xs (y + 1)
