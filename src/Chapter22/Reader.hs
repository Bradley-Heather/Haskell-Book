module Reader where

import Control.Applicative
import Data.Char

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop 

boopDoop ::  Integer -> Integer
boopDoop = do 
    a <- boop 
    b <- doop
    return (a + b)

----------------------------------

cap :: [Char] -> [Char]
cap xs = map toUpper xs 

rev :: [Char] -> [Char]
rev xs = reverse xs 

composed :: [Char] -> [Char]
composed = cap . rev 

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev 

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> rev <*> cap

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
    a <- rev 
    b <- cap
    return (a, b)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = rev >>= (\r -> cap >>= (\c -> return (r, c)))

------------------------------------

newtype Reader r a = Reader { runReader  :: r -> a }

instance Functor (Reader r) where 
    fmap f (Reader ra) = Reader $ \r -> f (ra r)
--  fmap f (Reader ra) = Reader $ (f . ra)

------------------------------------

ask :: Reader a a 
ask = undefined