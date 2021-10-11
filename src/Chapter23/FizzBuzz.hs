module FizzBuzz where 

import Control.Monad
import Control.Monad.Trans.State 

fizzBuzz :: Integer -> String
fizzBuzz n 
   | n `mod` 15 == 0 = "FizzBuzz"
   | n `mod` 3 == 0  = "Fizz"
   | n `mod` 5 == 0  = "Buzz"
   | otherwise       = show n

-- main :: IO ()
-- main = mapM_  ( putStrLn . fizzBuzz )  [1 .. 100] 

--------------------------
-- With State 

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo a b = fizzbuzzList $ enumFromThenTo b (b - 1) a

main :: IO ()
main = mapM_ putStrLn $ fizzbuzzFromTo 1 100