{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Exercises where

import Data.List
import Data.Char

data DividedResult = Result (Integer, Integer) | DividedByZero 
     deriving (Show, Eq)

divideBy :: Integer -> Integer -> DividedResult
divideBy 0 _ = DividedByZero
divideBy _ 0 = DividedByZero
divideBy num denom =  Result (go num denom 0)
   where go n d count 
          | n < d = (count, n)
          | otherwise =
                go (n - d) d (count + 1)

mc91 :: Integer -> Integer
mc91 n 
   | n > 100   = n - 10
   | otherwise = 91 

digitToWord :: Int -> String 
digitToWord n = ["Zero", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine"] !! n

digits :: Int -> [Int]
digits n = map digitToInt $ show n

wordNumber :: Int -> String
wordNumber n = intercalate "-" . map digitToWord $ digits n

