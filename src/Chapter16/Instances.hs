module Instances where

import Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
functorCompose (Fun _ f) (Fun _ g) x =
  fmap g (fmap f x) == fmap (g . f) x

-----------------------------------
-- 1.

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
      a <- arbitrary
      return (Identity a)

type IDCompose = Fun Int Int -> Fun Int Int -> Identity Int -> Bool

----------------------------------
-- 2.

data Pair a =
  Pair a a
  deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance Arbitrary a => Arbitrary (Pair a) where 
    arbitrary = do 
        a <- arbitrary
        return (Pair a a)

type PairCompose = Fun Int Int -> Fun Int Int -> Pair Int -> Bool

----------------------------------
-- 3.

data Two a b =
  Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where 
    arbitrary = do 
        a <- arbitrary
        b <- arbitrary
        return (Two a b)

type TwoCompose = Fun String Int -> Fun Int Int -> Two Int String -> Bool

----------------------------------
-- 4.

data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance  (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where 
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (Three a b c)

type ThreeCompose = Fun String Int -> Fun Int Int -> Three Int Int String -> Bool

-----------------------------
-- 5.

data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

------------------------------
-- 6.

data Four a b c d =
  Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

-------------------------------
-- 7.

data Four' a b =
  Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

-------------------------------
-- 8. 

-- Impossible with Trivial due to Kind

main :: IO ()
main = do
    putStrLn "Identity"
    quickCheck (functorCompose :: IDCompose)
    quickCheck (functorIdentity :: Identity Int -> Bool) 
    putStrLn "Pair"
    quickCheck (functorCompose :: PairCompose)
    quickCheck (functorIdentity :: Pair Int -> Bool)
    putStrLn "Two"
    quickCheck (functorCompose :: TwoCompose)
    quickCheck (functorIdentity :: Two Int String -> Bool)
    putStrLn "Three"
    quickCheck (functorCompose :: ThreeCompose)
    quickCheck (functorIdentity :: Three Int Int String -> Bool)