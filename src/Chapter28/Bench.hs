module Bench where 

import Criterion.Main
import Debug.Trace

{-
infixl 9 !?

_      !? n | n < 0 = Nothing
[]     !? _         = Nothing 
(x:_)  !? 0         = Just x 
(_:xs) !? n         = xs !? (n-1)
-}

infixl 9 !?

{-# INLINABLE (!?) #-}

(!?) :: [a] -> Int -> Maybe a
xs !? n 
   | n < 0 = Nothing
   | otherwise = 
       foldr 
       (\x r k -> 
           case k of 
               0 -> Just x 
               _ -> r (k-1))
       (const Nothing) xs n 

myList :: [Int]
myList = trace "myList was evaluated" 
    ([1..9999])

main :: IO ()
main = defaultMain
   [ bench "index list 9999"
     $ whnf (myList !!) 9998
   , bench "map list 9999"
     $ nf (map (+1)) myList   
   , bench "index list maybe index 9999"
     $ nf (myList !?) 9998 ]