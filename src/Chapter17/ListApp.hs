module ListApp where 

import Control.Applicative
import Data.Monoid 
import Test.QuickCheck          
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) 
   deriving (Eq, Show)

append :: List a -> List a -> List a 
append Nil ys = ys 
append (Cons x xs) ys = Cons x $ append xs ys 

fold :: (a -> b -> b) -> b -> List a -> b 
fold _ b Nil = b 
fold f b (Cons x xs) = f x (fold f b xs)

concat' :: List (List a) -> List a 
concat' = fold append Nil 

flatMap :: (a -> List b) -> List a -> List b
flatMap f xs = concat' (fmap f xs) 

instance Functor List where 
    fmap _ Nil      = Nil 
    fmap f (Cons a as) = Cons (f a) (fmap f as) 

instance Applicative List where 
    pure x               = Cons x Nil  
    (<*>) _ Nil          = Nil
    (<*>) Nil _          = Nil
    (<*>) (Cons a as) xs = append (a <$> xs) (as <*> xs)

instance Arbitrary a => Arbitrary (List a) where 
    arbitrary = do 
        a <- arbitrary
        frequency [ (1, return Nil)
                  , (3, return $ Cons a Nil)]

instance Eq a => EqProp (List a) where 
    (=-=) = eq

------------------------------

newtype ZipList' a = ZipList' [a]
   deriving (Eq, Show)

instance Functor ZipList' where 
    fmap f (ZipList' xs) = ZipList' $ fmap f xs 

instance Applicative ZipList'  where 
    pure x = ZipList' [x] 
    (<*>) (ZipList' xs) (ZipList' ys) = ZipList' (xs <*> ys)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs'`eq` ys'
    where xs' = let (ZipList' l) = xs
                in take 500 l
          ys' = let (ZipList' l) = ys
                in take 500 l

runQc :: IO ()
runQc = do 
    let triggerList = undefined :: List (Int, String, Int)
    putStrLn "List"
    quickBatch $ applicative triggerList
    let trigger = undefined :: ZipList' (Int, String, Int)
    putStrLn "ZipList"
    quickBatch $ applicative trigger