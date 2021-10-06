module Exercises where

import Test.QuickCheck

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = a <> (b <> c) == (a <> b) <> c 

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = a <> (b <> c) == (a <> b) <> c  

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidLeftIdentity a = (mempty <> a) == a 

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidRightIdentity a = (a <> mempty) == a 

-----------------------------------
-- 1. Trivial

data Trivial = Trivial deriving (Eq, Show) 

instance Semigroup Trivial where 
    (<>) Trivial Trivial = Trivial 

instance Monoid Trivial where 
    mempty  = Trivial 
    mappend = (<>)

instance Arbitrary Trivial where 
    arbitrary = return Trivial 

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool 

------------------------------------
-- 2. 

newtype Identity a = Identity a 
   deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where 
    (<>) (Identity a) (Identity b) = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where 
    mempty  = Identity mempty 
    mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where 
    arbitrary = do 
        a <- arbitrary
        return (Identity a)

type IDAssoc = Identity String -> Identity String -> Identity String -> Bool 

-------------------------------------
-- 3. 

data Two a b = Two a b 
   deriving (Eq, Show) 

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where 
    (<>) (Two a b) (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where 
    mempty  = Two mempty mempty 
    mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where 
    arbitrary = do 
        a <- arbitrary
        b <- arbitrary
        return (Two a b)

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool 

main :: IO ()
main = do
    putStrLn "Trivial"
    quickCheck (semigroupAssoc :: TrivAssoc) 
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)
    putStrLn "Identity"
    quickCheck (semigroupAssoc :: IDAssoc)
    quickCheck (monoidLeftIdentity :: Identity String -> Bool)
    quickCheck (monoidRightIdentity :: Identity String -> Bool)
    putStrLn "Two"
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (monoidLeftIdentity :: Two String String -> Bool)
    quickCheck (monoidRightIdentity :: Two String String -> Bool)

