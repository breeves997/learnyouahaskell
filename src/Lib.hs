module Lib
    ( someFunc,
      doubleMe,
      doubleSmall,
      tripleMe,
      quickSort
    ) where

doubleMe x = x + x
tripleMe x = x + x + x
addOne = (+1)

doubleSmall x = if x > 100
                then x
                else x*2

boomBangs xs = [ if x < 10 then "Boom!" else "Bang" | x <- xs, odd x]

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in quickSort smallerOrEqual ++ [x] ++ quickSort larger

quickSort' :: (Ord a) => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) =
    let smallerOrEqual = filter (<= x) xs
        larger = filter (> x) xs
    in quickSort' smallerOrEqual ++ [x] ++ quickSort larger

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

--findMissing :: Integral n => [n] -> n
--findMissing all@(x:y:xs) = head [a | (a, b) <- (zip [x,y..(last xs)] all), a /= b]
--findMissing' all@(x:y:xs) = [(a,b) | (a, b) <- (zip [x,y..(last xs)] all), a /= b]
--
--findMissing'' :: Integral n => [n] -> n
--findMissing'' nums
--  | length mismatched > 0 = head mismatched
--  | otherwise = -1
--  where mismatched = getMismatched nums
--getMismatched all@(x:y:xs) = [a | (a, b) <- (zip [x,y..(last xs)] all), a /= b]

findMissing :: Integral n => [n] -> n
findMissing nums@(x:y:xs) = let fullSeq = [x,y..(last xs)] in missingVal nums fullSeq
  
missingVal xs ys
  | (head xs) /= (head ys) = head xs
  | otherwise = missingVal (tail xs) (tail ys)

getPrecision :: [Double] -> Double -> Int -> (Double,Int)
getPrecision (x:y:xs) epsilon n
    | abs (x - pi) <= epsilon = (x, n)
    | otherwise = getPrecision ((x+y) : xs) epsilon (n+1)

leibSeq = let denom = [1,3..]
              num = everySecond (*(-1)) ( repeat 4)
          in zipWith (/) num denom
              
everySecond :: (a -> a) -> [a] -> [a]
everySecond f = loop
  where
    loop []  = []
    loop [x] = [x]
    loop (x:s:xs) = x : f s : loop xs

someFunc :: IO ()
someFunc = putStrLn "someFunc"
