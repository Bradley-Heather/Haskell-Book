module Unfold where

import Data.List 

mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs 
    where go :: Num a => a -> [a] -> a 
          go n [] = n 
          go n (x:xs) = go (n + x) xs

niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0

mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs 
    where go :: Num a => a -> [a] -> a 
          go n [] = n
          go n (x:xs) = go (n * x) xs

niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1

mehConcat :: [[a]] -> [a]
mehConcat xs = go [] xs 
     where go :: [a] -> [[a]] -> [a]
           go xs' [] = xs'
           go xs' (x:xs) = go (xs' ++ x) xs

niceConcat :: [[a]] -> [a]
niceConcat = foldr (++) []

--------------------------------------------

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x) 

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f n = 
      case f n of 
            Just (x, y) -> x : myUnfoldr f y 
            Nothing     -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x)) 

---------------------------------------------
-- | Binary Tree

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
   deriving (Eq, Ord, Show) 

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = 
      case f x of 
            Just (left, a, right) -> Node (unfold f left) a (unfold f right) 
            Nothing               -> Leaf

treeBuild :: Integer -> BinaryTree Integer 
treeBuild n = unfold tree 0 
     where 
           tree a
              | a < n     = Just (a + 1, a, a + 1)
              | otherwise = Nothing