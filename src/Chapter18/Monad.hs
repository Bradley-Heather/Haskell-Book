module Monad where 

import Control.Monad (join, liftM2)
import Control.Applicative 
import Test.QuickCheck           
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

bind :: Monad m => (a -> m b) -> m a -> m b 
bind a as = join (a <$> as)

-- join :: Monad m => m (m a) -> m a

-- fmap :: Functor f => (a -> b) -> f a -> f b

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b  

-- (=<<) :: Monad m => (a -> m b) -> m a -> m b

---------------------------------------------

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do 
    x <- xs 
    if even x 
        then [x, x*x]
        else [x]

-----------------------------------------------

type Founded = Int
type Coders = Int

data SoftwareShop =
  Shop
    { founded :: Founded
    , programmers :: Coders
    } deriving (Eq, Show)

data FoundedError =
    NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0     = Left $ NegativeYears n
  | n > 500   = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0     = Left $ NegativeCoders n
  | n > 500   = Left $ TooManyCoders n
  | otherwise = Right n

validateShop :: Int -> Int -> Either FoundedError SoftwareShop
validateShop y p =
  if p > div y 10
    then Left $ TooManyCodersForYears y p
    else Right $ Shop y p

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  programmers <- validateCoders coders
  validateShop founded programmers

-----------------------------------------------

data Sum a b = First a | Second b 
   deriving (Show, Eq)

instance Functor (Sum a) where
    fmap _ (First a)  = First a
    fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where 
    pure = Second 
    Second a <*> Second b = Second (a b)
    First  a <*> _        = First a
    _        <*> First a  = First a 

instance Monad (Sum a) where 
    return = Second 
    Second a >>= b = b a
    First  a >>= _ = First a 

--------------------------------------------------
-- 1.

data Nope a = NopeDotJpeg 
   deriving (Eq, Show)

instance Functor Nope where 
    fmap _ NopeDotJpeg = NopeDotJpeg

instance Applicative Nope where 
    pure _ = NopeDotJpeg
    NopeDotJpeg <*> NopeDotJpeg = NopeDotJpeg

instance Monad Nope where 
    return = pure 
    NopeDotJpeg >>= _ = NopeDotJpeg

instance Arbitrary (Nope a) where 
    arbitrary = return NopeDotJpeg

instance EqProp (Nope a) where
    (=-=) = eq 

type ISI = (Int, String, Int) 

triggerNope = undefined :: Nope ISI

---------------------------------------------------
-- 2.

data BahEither b a = PLeft a | PRight b
   deriving (Eq, Show)

instance Functor (BahEither b) where 
    fmap _ (PRight b) = PRight b 
    fmap f (PLeft a)  = PLeft (f a)

instance Applicative (BahEither b) where 
    pure                  = PLeft 
    PLeft a  <*> PLeft a' = PLeft (a a')
    _        <*> PRight b = PRight b 
    PRight b <*> _        = PRight b

instance Monad (BahEither b) where
    return = pure 
    PLeft  a >>= n  = n a 
    PRight b >>= _  = PRight b 

instance (Arbitrary b, Arbitrary a) => Arbitrary (BahEither b a) where 
    arbitrary = do
        b <- arbitrary
        a <- arbitrary
        frequency [ (1, return $ PLeft a)
                  , (2, return $ PRight b)]

instance (Eq b, Eq a) => EqProp (BahEither b a) where 
    (=-=) = eq 

triggerBahEither = undefined :: BahEither String ISI

--------------------------------------------------
-- 3.

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

instance Arbitrary a => Arbitrary (Identity a) where 
    arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where 
    (=-=) = eq 

triggerID = undefined :: Identity ISI

---------------------------------------------------
-- 4.

data List a = Nil | Cons a (List a) 
   deriving (Eq, Show)

instance Functor List where 
    fmap _ Nil         = Nil 
    fmap f (Cons a xs) = Cons (f a) (fmap f xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Applicative List where 
    pure n             = Cons n Nil 
    _ <*> Nil          = Nil 
    Nil <*> _          = Nil 
    Cons a as <*> xs = append (a <$> xs) (as <*> xs)

instance Monad List where
    return           = pure 
    Nil       >>= _  = Nil
    Cons a xs >>= f  = f a `append` (xs >>= f)

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

triggerList = undefined :: List ISI
------------------------------------------------------

runQc :: IO ()
runQc = do 
    putStrLn "Nope:"
    quickBatch $ functor triggerNope
    quickBatch $ applicative triggerNope
    quickBatch $ monad triggerNope 
    putStrLn ""
    putStrLn "BahEither"
    quickBatch $ functor triggerBahEither
    quickBatch $ applicative triggerBahEither
    quickBatch $ monad triggerBahEither
    putStrLn ""
    putStrLn "Identity"
    quickBatch $ functor triggerID
    quickBatch $ applicative triggerID
    quickBatch $ monad triggerID
    putStrLn ""
    putStrLn "List"
    quickBatch $ functor triggerList
    quickBatch $ applicative triggerList
    quickBatch $ monad triggerList

-------------------------------------------------------

j :: Monad m => m (m a) -> m a 
j = join   

l1 :: Monad m => (a -> b) -> m a -> m b 
l1 = fmap 

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c 
l2 = liftM2 

a :: Monad m => m a -> m (a -> b) -> m b 
a = undefined 

meh :: Monad m => [a] -> (a -> m b) -> m [b] 
meh = undefined 

flipType :: Monad m => [m a] -> m [a]
flipType = undefined 

