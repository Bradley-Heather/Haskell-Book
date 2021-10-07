module Exercises where

import Control.Applicative
import Data.List (elemIndex)
import Data.Monoid 
import Test.QuickCheck           
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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

-- 1.
added :: Maybe Integer 
added = (+3) <$> lookup 3 (zip [1,2,3][4,5,6])

-- 2.
y :: Maybe Integer 
y = lookup 3 $ zip [1,2,3][4,5,6]

z :: Maybe Integer 
z = lookup 2 $ zip [1,2,3][4,5,6] 

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3.
x :: Maybe Int 
x = elemIndex 3 [1,2,3,4,5]

n :: Maybe Int 
n = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int 
max' = max 

maxed :: Maybe Int 
maxed = max' <$> x <*> n

-- 4.
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

-------------------------------------------
-- Maybe 

validateLength :: Int -> String -> Maybe String 
validateLength maxLen s = if length s > maxLen 
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

------------------------------------------
-- Instances
-- 1. 

data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b) 

instance Applicative Pair where 
    pure a = Pair a a  
    Pair a b <*> Pair c d = Pair (a c) (b d)

instance Arbitrary a => Arbitrary (Pair a) where 
    arbitrary = do 
        a <- arbitrary
        return (Pair a a)

instance Eq a => EqProp (Pair a) where 
    (=-=) = eq

triggerPair = undefined :: Pair (Int, String, Int)

runQc :: IO ()
runQc =  quickBatch $ applicative triggerPair

------------------------------------------
-- 2.

data Two a b = Two a b 
   deriving (Eq, Show)

instance Functor (Two a) where 
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where 
    pure = Two mempty
    Two a b <*> Two a' b' = Two (a <> a') (b b')     

------------------------------------------
-- 3.

data Three a b c = Three a b c 
   deriving (Eq, Show)

instance Functor (Three a b) where 
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where 
    pure = Three mempty mempty 
    Three a b c <*> Three a' b' c' = Three (a <> a') (b <> b') (c c')

------------------------------------------
-- 4. 

data Three' a b = Three' a b b 
   deriving (Eq, Show)

instance Functor (Three' a) where 
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where 
    pure b = Three' mempty b b
    Three' a b c <*> Three' a' b' c' = Three' (a <> a') (b b') (c c')

-- combinations

stops :: String 
stops = "pbtdkg"

vowels :: String
vowels = "aieou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos a b c= (,,) <$> a <*> b <*> c