module Palindrome where

import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower)

palindrome = forever $ do 
  line1 <- getLine 
  case (line1 == reverse line1) of 
    True -> putStrLn "It's a Palindrome!!"
    False -> putStrLn "Nope!" 
    
simple :: String -> String
simple = filter (`elem` ['a'..'z']) . map toLower 


palindrome' = forever $ do 
  line1 <- getLine 
  if simple line1 == (reverse . simple) line1 then putStrLn "It's a Palindrom!!"
  else do 
    putStrLn "Nope!!"
    exitSuccess 
