module Exercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes
import Data.Monoid
import Control.Applicative

---------------------------
-- | Traversable Instances
-- Identity

newtype Identity a = Identity a 
   deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where 
    pure                       = Identity
    Identity a <*> Identity a' = Identity (a a') 

instance Monad Identity where 
    return           = pure 
    Identity a >>= n = n a 

instance Foldable Identity where 
    foldMap f (Identity a) = f a

instance Traversable Identity where 
    traverse f (Identity a) = Identity <$> f a 

instance Arbitrary a => Arbitrary (Identity a) where 
    arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where 
    (=-=) = eq  

------------------------------
-- Constant

newtype Constant a b = Constant { getConstant :: a}
   deriving (Eq, Show)

instance Functor (Constant a) where 
    fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where 
    foldMap f (Constant a) = mempty 

instance Traversable (Constant a) where 
    traverse _ (Constant a) = pure $ Constant a 

instance Arbitrary a => Arbitrary (Constant a b) where 
    arbitrary = Constant <$> arbitrary

instance (Eq a, Eq b) => EqProp (Constant a b) where 
    (=-=) = eq 

-----------------------------------
-- Maybe

data Optional a = Nada | Yep a 
   deriving (Eq, Show)

instance Functor Optional where 
    fmap _ Nada    = Nada 
    fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
    foldMap _ Nada    = mempty
    foldMap f (Yep a) = f a 

instance Traversable Optional where 
    traverse _ Nada    = pure Nada
    traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where 
    arbitrary = do 
        a <- arbitrary
        frequency [ (1, return Nada) 
                  , (3, return $ Yep a) ]

instance Eq a => EqProp (Optional a) where 
    (=-=) = eq  

-----------------------------------
-- List

data List a = Nil | Cons a (List a)
   deriving (Eq, Show)

instance Functor List where 
    fmap _ Nil         = Nil 
    fmap f (Cons a as) = Cons (f a) (f <$> as)

instance Foldable List where 
    foldMap _ Nil         = mempty 
    foldMap f (Cons a as) = f a <> foldMap f as

instance Traversable List where 
    traverse _ Nil         = pure Nil 
    traverse f (Cons a as) = Cons <$> f a <*> traverse f as

genList :: Arbitrary a => Gen (List a)
genList = do
    a <- arbitrary
    l <- genList 
    frequency [ (1, return Nil)
              , (3, return $ Cons a l)] 

instance Arbitrary a => Arbitrary (List a) where 
    arbitrary = genList 

instance Eq a => EqProp (List a) where
    (=-=) = eq

-----------------------------------
-- Three 

data Three a b c = Three a b c 
   deriving (Eq, Show)

instance Functor (Three a b) where 
    fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where 
    foldMap f (Three a b c) = f c

instance Traversable (Three a b) where 
    traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where 
    arbitrary = do 
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c 

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where 
    (=-=) = eq  

-----------------------------------
-- Pair 

data Pair a b = Pair a b 
   deriving (Eq, Show)

instance Functor (Pair a) where 
    fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
    foldMap f (Pair a b) = f b

instance Traversable (Pair a) where 
    traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where 
    arbitrary = do 
        a <- arbitrary
        Pair a <$> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where 
    (=-=) = eq 

-----------------------------------
-- Big 

data Big a b = Big a b b 
   deriving (Eq, Show)

instance Functor (Big a) where 
    fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where 
    foldMap f (Big a b b') = f b <> f b'

instance Traversable (Big a) where 
    traverse f (Big a b b') = Big a <$> f b <*> f b'

-----------------------------------
-- Bigger 

data Bigger a b = Bigger a b b b 
   deriving (Eq, Show)

instance Functor (Bigger a) where 
    fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'') 

instance Foldable (Bigger a) where 
    foldMap f (Bigger a b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where 
    traverse f (Bigger a b b' b'') = undefined

-----------------------------------
-- Tree 

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
   deriving (Eq, Show)

instance Functor Tree where 
    fmap _ Empty          = Empty
    fmap f (Leaf a)       = Leaf (f a)
    fmap f (Node as a bs) = Node (fmap f as) (f a) (fmap f bs)

instance Foldable Tree where 
    foldMap _ Empty          = mempty 
    foldMap f (Leaf a)       = f a 
    foldMap f (Node as a bs) = foldMap f as <> f a <> foldMap f bs

instance Traversable Tree where 
    traverse _ Empty          = pure Empty 
    traverse f (Leaf a)       = Leaf <$> f a
    traverse f (Node as a bs) = Node <$> traverse f as <*> f a <*> traverse f bs 

treeGen :: Arbitrary a => Gen (Tree a)
treeGen = do
       as <- treeGen
       bs <- treeGen
       a <- arbitrary
       frequency [ (1, return Empty)
                 , (2, return $ Leaf a)
                 , (3, return $ Node as a bs) ]

instance Arbitrary a => Arbitrary (Tree a) where 
    arbitrary = treeGen

instance Eq a => EqProp (Tree a) where 
    (=-=) = eq

-- Tests:

type Trigger = (Int, Int, [Int])
trigger = undefined

runQc :: IO ()
runQc = do 
    quickBatch $ traversable (trigger :: Identity Trigger) 
    quickBatch $ traversable (trigger :: Constant Trigger Trigger)
    quickBatch $ traversable (trigger :: Optional Trigger)
    quickBatch $ traversable (trigger :: List Trigger)
    quickBatch $ traversable (trigger :: Three Trigger Trigger Trigger)
    quickBatch $ traversable (trigger :: Pair Trigger Trigger)
    quickBatch $ traversable (trigger :: Tree Trigger)