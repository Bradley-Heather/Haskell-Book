module Applicative where

import Control.Applicative
import Data.List (elemIndex)
import Data.Monoid 
-- import Test.QuickCheck 
-- import Test.QuickCheck.Checkers
-- import Test.QuickCheck.Classes

f x = 
    lookup x [ (3, "hello")
             , (4, "julie")
             , (5, "kbai")
             ]

g y = 
    lookup y [ (7, "sup")
             , (8, "chris")
             , (9, "aloha")
             ]

h z =
    lookup z [(2,3), (5,6), (7,8)]

m x = 
    lookup x [(4, 10), (8, 13), (1, 9001)]

-----------------------------------
-- Type Check Exercise

-- 1)
added :: Maybe Integer 
added = (+3) <$> lookup 3 (zip [1,2,3][4,5,6])

-- 2) 
y :: Maybe Integer 
y = lookup 3 $ zip [1,2,3][4,5,6]

z :: Maybe Integer 
z = lookup 2 $ zip [1,2,3][4,5,6] 

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3)
x :: Maybe Int 
x = elemIndex 3 [1,2,3,4,5]

n :: Maybe Int 
n = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int 
max' = max 

maxed :: Maybe Int 
maxed = max' <$> x <*> n

-- 4)
as = [1,2,3]
bs = [4,5,6]

a :: Maybe Integer 
a = lookup 3 $ zip as bs 

b :: Maybe Integer 
b = lookup 2 $ zip as bs 

summed :: Maybe Integer 
summed = fmap sum $ (,) <$> x <*> y 

-------------------------------------------------

newtype Identity a = Identity a 
   deriving (Eq, Ord, Show)

instance Functor Identity where 
    fmap f (Identity a) = Identity (f a) 

instance Applicative Identity where 
    pure  = Identity 
    Identity b <*> Identity a = Identity (b a) 

--------------------------------------------------

newtype Constant a b = Constant { getConstant :: a }
   deriving (Eq, Ord, Show)

instance Functor (Constant a) where 
    fmap f (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where 
    pure _ = Constant mempty
    (<*>) (Constant a) (Constant b) = Constant (mappend a b) 

-----------------------------------------------------

fixer = const <$> Just "Hello" <*> Just "World"

fixer2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1, 2, 3]

------------------------------------------------------

data List a = Nil | Cons a (List a) 
   deriving (Eq, Show)

append :: List a -> List a -> List a 
append Nil ys = ys 
append (Cons x xs) ys = Cons x $ append xs ys 

fold :: (a -> b -> b) -> b -> List a -> b 
fold _ b Nil = b 
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a 
concat' = fold append Nil 

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = undefined

instance Functor List where 
    fmap _ Nil      = Nil 
    fmap f (Cons a rest) = Cons (f a) (fmap f rest) 

instance Applicative List where 
    pure x               = Cons x Nil  
    (<*>) _ Nil          = Nil
    (<*>) Nil _          = Nil
    (<*>) (Cons a as) xs = undefined

--------------------------------------------

newtype ZipList' a = ZipList' [a]
   deriving (Eq, Show)

instance Functor ZipList' where 
    fmap f (ZipList' xs) = ZipList' $ fmap f xs 

instance Applicative ZipList'  where 
    pure x = ZipList' [x] 
    (<*>) = undefined 

-------------------------------------------

data Validation e a = Failure e | Success a 
   deriving (Eq, Show)

instance Functor (Validation e) where 
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
    pure  = undefined 
    (<*>) = undefined   

-------------------------------------------
-- Maybe 

validateLength :: Int -> String -> Maybe String 
validateLength maxLen s = if (length s) > maxLen 
    then Nothing 
    else Just s 

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = Name <$> validateLength 25 s 

mkAddress :: String -> Maybe Address
mkAddress a = Address <$> validateLength 100 a 

data Person = Person Name Address deriving (Eq, Show) 

{-
mkPerson :: String -> String -> Maybe Person 
mkPerson n a = 
    case mkName n of 
        Nothing -> Nothing 
        Just n' -> 
            case mkAddress a of 
                Nothing -> Nothing 
                Just a' -> 
                    Just $ Person n' a' 
-}
 
mkPerson :: String -> String -> Maybe Person
mkPerson n a = Person <$> mkName n <*> mkAddress a

mkPerson' :: String  -> String -> Maybe Person
mkPerson' n a = liftA2 Person (mkName n) (mkAddress a)