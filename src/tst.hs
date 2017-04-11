module Scrum () where

wat x = succ x

--Hey dude you can add all the predicates you want so long as you separate them with a comma! neato!
boomBangs xs = [ if x < 10 then "Boom!" else "Bang" | x <- xs, odd x]

nouns = ["hobo", "frog", "pope"]
adjectives = ["lazy", "grouchy", "scheming"]
combos = [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]

findMissing :: Integral n => [n] -> n
findMissing nums@(x:y:xs) = let fullSeq = [x,y..(last xs)] in missingVal nums fullSeq
  
missingVal xs ys
  | head xs /= head ys = head xs
  | otherwise missingVal tail xs tail ys
