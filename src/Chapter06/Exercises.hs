module Exercises where

import Data.List

---------------------------------
-- | Does it TypeCheck...

x :: Int -> Int
x blah = blah + 20

printIt :: IO ()
printIt = putStrLn $ show (x 10)

--------------------
data Person = Person Bool deriving (Show)

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

--------------------
data Mood =
    Blah
  | Woot
  deriving (Show, Eq)

settleDown :: Mood -> Mood
settleDown x =
  if x == Woot
  then Blah
  else x

---------------------
type Subject = String
type Verb = String
type Object = String


data Sentence =
  Sentence Subject Verb Object
  deriving (Show, Eq)

s1 = Sentence "Dog" "Drool"
s2 = Sentence "Julie" "Loves" "Dogs"

-------------------------------
-- | What to do 

data Rocks =
  Rocks String deriving (Show, Eq)

data Yeah =
  Yeah Bool deriving (Show, Eq)

data Papu =
  Papu Rocks Yeah
  deriving (Show, Eq)

-- phew = Papu "Chases" True 

truth = Papu (Rocks "chomsky") (Yeah True)

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

-- comparePapus :: Papu -> Papu -> Bool 
-- comparePapus p p' = p > p' -- No Ord Instance

------------------------
-- | Type-Kwon-Do

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b  = f a == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f a b = undefined