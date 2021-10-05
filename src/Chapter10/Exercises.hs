module Exercises where 

-- | Rewriting functions using folds...

myOr :: [Bool] -> Bool
myOr = foldr (||) False 

myAny :: (a -> Bool) -> [a] -> Bool 
myAny n = foldr go False 
    where go x y = n x || y

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool 
myElem n = foldr go False 
    where go x y = n == x || y 

myElem' :: Eq a => a -> [a] -> Bool 
myElem' n = myAny (== n)    

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr go []
     where go x y = if f x then x : y else y

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' f = foldr (\x y -> if f x then x : y else y) [] 

squish :: [[a]] -> [a]
squish = foldr (++) [] 

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr (\a b -> if f a b == GT then a else b) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldr (\a b -> if f a b == LT then a else b) x xs

myMaximum :: (Ord a) => [a] -> a 
myMaximum = myMaximumBy compare 

myMinimum :: (Ord a) => [a] -> a 
myMinimum = myMinimumBy compare 