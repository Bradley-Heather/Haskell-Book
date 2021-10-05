import Data.List
import Distribution.SPDX (LicenseId(XSkat))

-- String Processing

replaceThe :: String -> String 
replaceThe = unwords . replaceThe' . words

replaceThe' :: [String] -> [String]
replaceThe' []     = []
replaceThe' (x:xs) 
       | isNothing (notThe x) = "a" : replaceThe' xs
       | otherwise            = x   : replaceThe' xs 


notThe :: String -> Maybe String
notThe x
    | x == "the" = Nothing 
    | otherwise  = Just x

vowels = "aieouAEIOU"

type Vowels     = Integer 
type Consonants = Integer

countTheBeforeVowel :: String -> Integer 
countTheBeforeVowel = countTheBeforeVowel' . words

countTheBeforeVowel' :: [String] -> Integer
countTheBeforeVowel' []  = 0
countTheBeforeVowel' [n] = 0
countTheBeforeVowel' (x:n:xs) 
       | isNothing (notThe x) && head n `elem` vowels  = 1 + countTheBeforeVowel' (n:xs)
       | otherwise                                     = countTheBeforeVowel' (n:xs) 

countVowels :: String -> Vowels
countVowels [] = 0
countVowels (x:xs) 
      | x `elem` vowels = 1 + countVowels xs
      | otherwise        = countVowels xs 

countCons :: String -> Consonants
countCons [] = 0
countCons (x:xs) 
      | x `elem` vowels || x == ' ' = countCons xs
      | otherwise                   = 1 + countCons xs 

-- Validate the Word

newtype Word' = Word' String 
    deriving (Eq, Show)

countBoth :: String -> (Vowels, Consonants)
countBoth n = (countVowels n, countCons n)

mkWord :: String -> Maybe Word'
mkWord n 
     | uncurry (>) (countBoth n) = Nothing  
     | otherwise = Just (Word' n)

-- It's Only Natural

data Nat = 
    Zero | Succ Nat 
    deriving (Eq, Show)

natToInteger :: Nat -> Integer 
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n 

integerToNat :: Integer -> Maybe Nat 
integerToNat 0 = Just Zero
integerToNat n 
      | n < 0  = Nothing 
      | otherwise = case integerToNat (n-1) of 
          Nothing -> Nothing 
          Just n -> Just (Succ n) 


-- Small Library for Maybe

isJust :: Maybe a -> Bool 
isJust Nothing   = False 
isJust (Just _)  = True

isNothing :: Maybe a -> Bool 
isNothing Nothing  = True 
isNothing (Just _) = False  

mayybee :: b -> (a -> b) -> Maybe a -> b 
mayybee n _ Nothing  = n  
mayybee _ f (Just a) = f a   

fromMaybe :: a -> Maybe a -> a
fromMaybe n Nothing  = n 
fromMaybe _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:xs) = Just x  

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just n) = [n]  

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : xs) = catMaybes xs 
catMaybes (Just x : xs) = x : catMaybes xs 

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing : xs) = Nothing 
flipMaybe (Just n : xs ) = case flipMaybe xs of
              Nothing -> Nothing 
              Just a -> Just (n : a) 

-- Small Library of Either

lefts' :: [Either a b] -> [a]
lefts' = foldr takeLeft []
    where takeLeft (Left x)  acc = x : acc 
          takeLeft (Right _) acc = acc

rights' :: [Either a b] -> [b]
rights' = foldr takeRight []
    where takeRight (Right x) acc = x : acc 
          takeRight (Left _) acc  = acc 

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' a =  (,) (lefts' a) (rights' a)

eitherMaybe :: (b -> c) -> Either a b -> Maybe c
eitherMaybe f (Right x) = Just (f x)
eitherMaybe f (Left _)  = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f1 _ (Left a) = f1 a
either' _ f2 (Right a) = f2 a 

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = undefined