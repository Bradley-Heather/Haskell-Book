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

-----------------------------------
-- 4. 

data Three a b c = Three a b c 
   deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where 
    (<>) (Three a b c) (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where 
    arbitrary = do 
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (Three a b c)

type S = String

type ThreeAssoc = Three S S S -> Three S S S -> Three S S S -> Bool

------------------------------------
-- 5. 

newtype BoolConj = BoolConj Bool 
   deriving (Eq, Show)

instance Semigroup BoolConj where 
    BoolConj True  <> BoolConj True  = BoolConj True 
    BoolConj _     <> BoolConj False = BoolConj False
    BoolConj False <> BoolConj _     = BoolConj False

instance Monoid BoolConj where 
    mempty  = BoolConj True 
    mappend = (<>)  

instance Arbitrary BoolConj where 
    arbitrary = do 
        frequency [ (1, return $ BoolConj True)
                  , (2, return $ BoolConj False)]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-------------------------------------
-- 6. 

newtype BoolDisj = BoolDisj Bool 
   deriving (Eq, Show)

instance Semigroup BoolDisj where 
    BoolDisj True  <> _              = BoolDisj True 
    BoolDisj False <> BoolDisj True  = BoolDisj True 
    BoolDisj False <> BoolDisj False = BoolDisj False 

instance Monoid BoolDisj where 
    mempty  = BoolDisj False
    mappend = (<>)  

instance Arbitrary BoolDisj where 
    arbitrary = do 
       frequency [ (1, return $ BoolDisj True)
                 , (3, return $ BoolDisj False)
                 ]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-------------------------------------
-- 7. 

data Or a b = Fst a | Snd b 
   deriving (Eq, Show)

instance Semigroup (Or a b) where 
    Fst a <> Fst a' = Fst a' 
    Fst a <> Snd b  = Snd b  
    Snd b <> _      = Snd b 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where 
     arbitrary = do 
         a <- arbitrary
         b <- arbitrary 
         frequency [ (2, return $ Fst a)
                   , (3, return $ Snd b)
                   ]

type OrAssoc = Or S S -> Or S S -> Or S S -> Bool 

-------------------------------------
-- 8. 

newtype Combine a b = Combine { unCombine :: a -> b }




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
    putStrLn "Three"
    quickCheck (semigroupAssoc :: ThreeAssoc)
    putStrLn "BoolConj"
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)
    putStrLn "BoolDisj"
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
    putStrLn "Or"
    quickCheck (semigroupAssoc :: OrAssoc)

