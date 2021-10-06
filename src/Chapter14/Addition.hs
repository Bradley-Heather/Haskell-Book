module Addition where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do 
  describe "Arithmatic" $ do 
    it "15 Divided by 3 is 5" $ do 
      dividedBY 15 3 `shouldBe` (5, 0) 
    it "22 divided by 5 is 4 remainder 2" $ do 
      dividedBY 22 5 `shouldBe` (4, 2) 
    it "3 multiplied by 6 is 18" $ do 
      prodSum 3 6 `shouldBe` 18
    it "x + 1 is always greater than x" $ do 
      property $ \x -> x + 1 > (x :: Int)

prop_additionGreater :: Int -> Bool 
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater 

dividedBY :: Integral a => a -> a -> (a,a) 
dividedBY num denom = go num denom 0 
   where go n d count 
           | n < d = (count, n) 
           | otherwise = go (n - d) d (count + 1)

prodSum :: (Eq a, Num a) => a -> a -> a 
prodSum a 0 = 0
prodSum a b = a + prodSum a (b - 1)

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool 
genBool' = elements [False, True] 

genOrdering :: Gen Ordering 
genOrdering = elements [LT,GT,EQ]

genChar :: Gen Char 
genChar = elements ['a' .. 'z'] 

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b) 
genEither = do 
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b] 

genMaybe :: Arbitrary a => Gen (Maybe a) 
genMaybe = do  
  a <- arbitrary 
  frequency [ (1, return Nothing), (3, return (Just a))] 