module Folds where

import Prelude hiding ( foldr
                      , foldl
                      , sum
                      , length
                      , reverse
                      , map )


foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ b []     = b
foldr f b (x:xs) = f x (foldr f b xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ b []     = b
foldl f b (x:xs) = foldl f b' xs
  where b' = f b x


sum :: [Int] -> Int
sum = foldr (+) 0


length :: [a] -> Int
length = foldr (const (+1)) 0
--length = foldr (\_ b -> b + 1) 0


minList :: [Int] -> Int
minList (x:xs) = foldr min x xs


maxList :: [Int] -> Int
maxList (x:xs) = foldr max x xs


reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

inits :: [a] -> [[a]]
inits = undefined
