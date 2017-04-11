module Codewars.BouncingBall where

bouncingBall :: Double -> Double -> Double -> Integer
bouncingBall h b w 
  | b >= 1 = -1
  | otherwise = fromIntegral $ 1 + 2 * (length . takeWhile (>= 1) $ unfold (* 0.3) 10)
  where unfold f x = x : unfold f (f x)

x = 1 + 2 * (length . takeWhile (>= 1) $ unfold (* 0.3) 10)

unfold f x = x : unfold f (f x)

