module Main where

import Test.QuickCheck 


-------------------------------------------
-- Semigroup

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial 

instance Arbitrary Trivial where
  arbitrary = return Trivial 


type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool 

-------------------------------------------

newtype Identity a = Identity a 
   deriving (Eq, Show)
 
instance Arbitrary a => Arbitrary  (Identity a) where
  arbitrary = do 
    x <- arbitrary
    return (Identity x)

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity b = Identity (a <> b)

type IdAssoc = Identity String -> Identity String -> Identity String -> Bool 

--------------------------------------------
 
data Two a b = Two a b 
    deriving (Eq, Show)
 
instance (Semigroup a, Semigroup b) => Semigroup  (Two a b) where 
  Two a b <> Two a' b' = Two (a <> a') (b <> b') 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary  = do 
    x <- arbitrary 
    y <- arbitrary 
    return (Two x y)

type TwoAssoc = Two Trivial Trivial -> Two Trivial Trivial -> Two Trivial Trivial -> Bool

--------------------------------------------- 

data Three a b c = Three a b c 
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where 
	 Three a b c <> Three a' b' c' = Three (a <> a') (b <> b') (c <> c') 

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where 
  arbitrary = do 
    x <- arbitrary
    y <- arbitrary 
    z <- arbitrary
    return (Three x y z)

type ThreeAssoc = Three Trivial Trivial Trivial 
                  -> Three Trivial Trivial Trivial 
                  -> Three Trivial Trivial Trivial 
                  -> Bool

-------------------------------------------

newtype BoolDisj = BoolDisj Bool 
   deriving (Eq, Show)

instance Semigroup BoolDisj where 
    BoolDisj True <> BoolDisj _      = BoolDisj True 
    BoolDisj False <> BoolDisj True  = BoolDisj True
    BoolDisj False <> BoolDisj False = BoolDisj False 

instance Arbitrary BoolDisj where 
    arbitrary = elements [BoolDisj True 
                         , BoolDisj False
                         ]  

type DisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool 

--------------------------------------------

data Or a b = Fst a | Snd b 
   deriving (Eq, Show) 

instance Semigroup (Or a b) where 
  Fst _ <> Snd b = Snd b 
  Fst _ <> Fst b = Fst b 
  Snd a <> Fst _ = Snd a 
  Snd a <> Snd _ = Snd a 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where 
  arbitrary = do 
    x <- arbitrary 
    y <- arbitrary  
    elements [Fst x, Snd y]

type OrAssoc = Or Trivial Trivial -> Or Trivial Trivial -> Or Trivial Trivial -> Bool 

------------------------------------------------
-- To Do

-- newtype Combine a b = Combine { unCombine :: a -> b }

-- newtype Comp a = Comp { unComp :: (a -> a)}

-- data Validation a b = Failure a | Success b deriving (Eq, Show)
-- instance Semigroup a => Semigroup (Validation a b) where

-------------------------------------------------

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool 
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c ) 

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidLeftIdentity a = (mempty <> a) == a 

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidRightIdentity a = (a <> mempty) == a 


main :: IO () 
main = do
  putStrLn "Trivial"
  checkTrivial
  putStrLn "Identity"
  checkIdentity
  putStrLn "Two"
  checkTwo
  putStrLn "DisjBool"
  quickCheck (semigroupAssoc :: DisjAssoc)
  putStrLn "Or"
  quickCheck (semigroupAssoc :: OrAssoc)

---------------------------------------------------
-- Monoid

instance Monoid Trivial where 
  mempty  = Trivial 
  mappend = (<>)

checkTrivial :: IO () 
checkTrivial = do
  let mli = monoidLeftIdentity
      mlr = monoidRightIdentity 
  putStrLn "Trivial"
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (mli :: Trivial -> Bool)
  quickCheck (mlr :: Trivial -> Bool)

---------------------------------------------------

instance Monoid a => Monoid (Identity a) where 
  mempty  = Identity mempty 
  mappend = (<>) 


checkIdentity :: IO () 
checkIdentity = do
  let mli = monoidLeftIdentity
      mlr = monoidRightIdentity 
  quickCheck (semigroupAssoc :: IdAssoc)
  quickCheck (mli :: Identity [Int] -> Bool)
  quickCheck (mlr :: Identity [Int] -> Bool)

----------------------------------------------------

instance (Monoid a, Monoid b) => Monoid (Two a b) where 
  mempty = Two mempty mempty 
  mappend = (<>)

checkTwo :: IO () 
checkTwo = do
  let mli = monoidLeftIdentity
      mlr = monoidRightIdentity 
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (mli :: Two [Int] String -> Bool)
  quickCheck (mlr :: Two [Int] String -> Bool)

----------------------------------------------------

-- To Do 

-- instance Monoid BoolDisj 
-- Combine 
-- Comp

-- Mem