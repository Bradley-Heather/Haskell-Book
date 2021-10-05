module Exercises where 

---------------------------------
-- | Thy Fearful Symmetry

td :: String -> [String]
td [] = []
td n  = if null dp then takeWhile (/=  ' ') n : td dp else takeWhile (/=  ' ') n : td (tail dp)
   where dp = dropWhile (/= ' ') n
      

firstSen = "Tiger Tiger, Burning Bright\n"
secondSen = "In the Forests of the Night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy Fearful symmetry"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines [] = []
myLines n
   | null dp = takeWhile (/= '\n') n : myLines dp
   | otherwise = takeWhile (/= '\n') n : myLines (tail dp)
          where dp = dropWhile (/= '\n') n

words' _ [] = []
words' n xs 
   | null dp = takeWhile (/= n) xs : words' n dp
   | otherwise = takeWhile (/= n) xs : words' n (tail dp)
          where dp = dropWhile (/= n) xs

------------------------------------------
-- | Square Cube

mySqr :: [Integer]
mySqr = [x^2 | x <- [1..5]]

myCube :: [Integer]
myCube = [y^3 | y <- [1..5]]

myTup ::  Int
myTup = length $ [(x, y) | x <- mySqr , y <- myCube, x < 50 , y < 50]

-----------------------------------------

acro :: String -> String
acro xs = [x | x <- xs , x `elem` ['A'..'Z']] 

itIsAMystery :: [Char] -> [Bool]
itIsAMystery xs = map (\x -> x `elem` "aeiou") xs

myFilter ::  String -> [String]
myFilter = filter (\x -> x `notElem` ["The", "a", "an"]) . words

-- | Zip

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (,) x y : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' n (x:xs) (y:ys) = n x y : zipWith' n xs ys 

-----------------------------------
-- | Writing standard Functions

myOr :: [Bool] -> Bool
myOr [] = False 
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool 
myAny _ [] = False 
myAny n (x:xs)
   | n x       = True 
   | otherwise = myAny n xs

myElem :: Eq a => a -> [a] -> Bool 
myElem _ [] = False
myElem n (x:xs) 
    | x == n    = True 
    | otherwise = myElem n xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs 

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap n [] = []
squishMap n (x:xs) = n x ++ squishMap n xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy n (x:xs) = go n xs x
     where 
         go f (b:bs) a =
           case f b a of 
               GT -> go f bs b
               LT -> go f bs a
               EQ -> go f bs b
         go f [] a = a 

myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy' _ [x] = x
myMaximumBy' n (x:y:xs) 
    | n x y == GT = myMaximumBy' n (x:xs)
    | otherwise   = myMaximumBy' n (y:xs)  

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy n (x:y:xs) 
    | n x y == LT = myMinimumBy n (x:xs)
    | otherwise   = myMinimumBy n (y:xs)  

myMaximum :: (Ord a) => [a] -> a 
myMaximum = myMaximumBy compare 

myMinimum :: (Ord a) => [a] -> a 
myMinimum = myMinimumBy compare 