module Exercises where 

import Test.QuickCheck
import Test.QuickCheck.Modifiers  (NonZero)
import Test.Hspec
import Data.List       (sort)

-- | Using QuickCheck

----------------------------
-- 1.

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = halfIdentity x == x

-----------------------------
-- 2. 

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = 
    snd $ foldr go (Nothing, True) xs 
    where go _ status@(_, False) = status 
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)   

prop_listOrdered :: String -> Bool
prop_listOrdered = listOrdered . sort   

------------------------------
-- 3. 

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool 
plusAssociative x y z = x + (y + z) == (x + y) + z 

plusCommutative :: (Eq a, Num a) => a -> a -> Bool 
plusCommutative x y = x + y == y + x  

prop_plusAssociative :: Int -> Int -> Int ->  Bool
prop_plusAssociative = plusAssociative

prop_plusCommutative :: Int -> Int -> Bool
prop_plusCommutative = plusCommutative

-------------------------------
-- 4.

multAssociative :: Int -> Int -> Int -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

multCommutative :: Int -> Int -> Bool
multCommutative x y = x * y == y * x

--------------------------------
-- 5.

-- | NonZero required so division by zero does not occur....
prop_quotRem :: NonZero Int -> NonZero Int -> Bool 
prop_quotRem (NonZero x) (NonZero y) = (quot x y) * y + (rem x y) == x 

prop_divMod :: NonZero Int -> NonZero Int -> Bool 
prop_divMod (NonZero x) (NonZero y) = (div x y) * y + (mod x y) == x 

--------------------------------
-- 6.
-- | Will fail!

powerAssociative :: Int -> Int -> Int -> Bool
powerAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

powerCommutative :: Int -> Int -> Bool
powerCommutative x y = x ^ y == y ^ x

--------------------------------
-- 7. 

prop_reverse :: String -> Bool
prop_reverse x = (reverse . reverse) x == id x

-------------------------------
-- To Do
-- 8.
-- 9.
-- 10.
-- 11.

runQc :: IO ()
runQc = do
  putStrLn "1. halfIdentity"
  quickCheck prop_halfIdentity
  putStrLn "2. listOrdered"
  quickCheck prop_listOrdered
  putStrLn "3. plusAssociative, plusCommutative"
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
  putStrLn "4. multAssociative, multCommutative"
  quickCheck multAssociative
  quickCheck multCommutative
  putStrLn "5. quotRem, divMod"
  quickCheck prop_quotRem 
  quickCheck prop_divMod 
  putStrLn "6. powerAssociative, powerCommutative"
  quickCheck powerAssociative
  quickCheck powerCommutative
  putStrLn "7. reverse"
  quickCheck prop_reverse 

