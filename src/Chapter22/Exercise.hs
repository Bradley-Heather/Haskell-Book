module Exercise where

import Data.Char ( toUpper )

cap :: [Char] -> [Char]
cap = map toUpper  

rev :: [Char] -> [Char] 
rev = reverse 

composed :: [Char] -> [Char] 
composed =  rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap 

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev 

tupled' :: [Char] -> ([Char], [Char])
tupled' = do 
    a <- cap 
    b <- rev 
    return (a,b)

  