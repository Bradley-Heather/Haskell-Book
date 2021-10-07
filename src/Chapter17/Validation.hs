module Validation where 

import Control.Applicative
import Data.Monoid 
import Test.QuickCheck          hiding (Failure, Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation err a = Failure err | Success a 
   deriving (Eq, Show)

instance Functor (Validation err) where 
    fmap _ (Failure err) = Failure err
    fmap f (Success a)   = Success (f a)

instance Monoid err => Applicative (Validation err) where
    pure                     = Success 
    Failure e <*> Failure e' = Failure $ e `mappend` e'
    Failure e <*> _          = Failure e  
    _         <*> Failure e  = Failure e 
    Success a <*> Success a' = Success $ a a'

instance (Arbitrary err, Arbitrary a) => Arbitrary (Validation err a ) where
    arbitrary = do 
        err <- arbitrary
        a   <- arbitrary
        frequency [ (2, return $ Failure err)
                  , (3, return $ Success a) ]

instance (Eq err, Eq a) => EqProp (Validation err a) where 
    (=-=) = eq

runQc :: IO ()
runQc = do 
    let trigger = undefined :: Validation String (Int, String, Int) 
    quickBatch $ applicative trigger