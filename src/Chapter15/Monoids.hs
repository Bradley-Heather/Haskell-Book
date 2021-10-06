{-# LANGUAGE FlexibleContexts #-}

module Monoids where

import Test.QuickCheck
import Data.Monoid

data Optional a = Nada | Only a 
   deriving (Eq, Show) 

instance Semigroup a => Semigroup (Optional a) where 
  (<>) Nada Nada = Nada 
  (<>) (Only a) Nada = Only a
  (<>) Nada (Only a) = Only a
  (<>) (Only a) (Only b) = Only (a <> b)

instance Monoid a => Monoid (Optional a) where
  mempty  = Nada  
  mappend = (<>)  

genOnly :: Arbitrary a => Gen (Optional a)
genOnly = Only <$> arbitrary

genOptional :: (Arbitrary a) => Gen (Optional a)
genOptional =
  frequency [ (1, return Nada)
            , (10, genOnly)
            ]

----------------------------------

newtype First' a = First' { getFirst' :: Optional a}
    deriving (Eq, Show) 

instance Semigroup (First' a) where 
    (<>) (First' Nada) (First' Nada )        = First' Nada 
    (<>) (First' Nada) (First' (Only a))     = First' (Only a)  
    (<>) (First' (Only a)) (First' Nada)     = First' (Only a) 
    (<>) (First' (Only a)) (First' (Only _)) = First' (Only a)

instance Monoid (First' a) where 
     mempty  = First' Nada
     mappend = (<>) 

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend 

instance Arbitrary a => Arbitrary (First' a) where 
  arbitrary = do 
  a <- arbitrary 
  frequency [ (1, return $ First' Nada)
            , (3, return $ First' $ Only a) 
            ] 

type FirstMappend = First' String -> First' String -> First' String -> Bool 

type FstId        = First' String -> Bool 

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = a <> (b <> c) == (a <> b) <> c  

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidLeftIdentity a = (mempty <> a) == a 

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidRightIdentity a = (a <> mempty) == a 


main :: IO () 
main = do 
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
