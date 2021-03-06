module Cipher where

import Data.Char (ord, chr, isAsciiLower, isAsciiUpper)
import Data.List (find)

-- Ceasar Cipher

cc :: Int -> String -> String
cc n = unwords . map (ceasarCipher n) . words

ceasarCipher :: Int -> String -> String
ceasarCipher n [] = []
ceasarCipher n (x:xs) 
    | ord x + n > 122 = chr (ord x - (26 - n))  : ceasarCipher n xs 
    | otherwise       = chr (ord x + n) : ceasarCipher n xs

uc :: Int -> String -> String
uc n = unwords . map (unCipher n) . words

unCipher :: Int -> String -> String
unCipher n [] = []
unCipher n (x:xs)
    | ord x - n < 97 = chr (ord x + (26 - n))  : unCipher n xs 
    | otherwise      = chr (ord x - n) : unCipher n xs

-- Vigenere Cipher 

rotateLetter :: Char -> Int -> Char
rotateLetter c r
  | isAsciiLower c = shift (ord 'a')
  | isAsciiUpper c = shift (ord 'A')
  | otherwise      = c
  where shift base = chr (base + ((ord c - base + r) `mod` len))
        len        = 1 + ord 'z' - ord 'a'

rotate :: String -> [Int] -> String
rotate []     _      = []
rotate xs     []     = xs
rotate (x:xs) (r:rs) = rotateLetter x r : rotate xs rs

rotation :: [Char] -> [Int]
rotation []        = []
rotation (k:ks)
  | isAsciiLower k = (ord k - ord 'a') : rotation ks
  | isAsciiUpper k = (ord k - ord 'A') : rotation ks
  | otherwise      = rotation ks

vCipher :: String -> String -> String
vCipher [] _  = []
vCipher xs [] = xs
vCipher xs ks = rotate xs (concat (repeat (rotation ks)))

vDecrypt :: String -> String -> String
vDecrypt [] _  = []
vDecrypt xs [] = xs
vDecrypt xs ks = rotate xs (concat (repeat (map negate (rotation ks))))

-- main :: Int  -> String -> IO ()
main = do 
  putStr "Please choose your cipher: "
  cipher <- getLine 
  putStrLn "What would you like to Cipher: "
  phrase <- getLine 
  return (vCipher phrase cipher)
  putStr "Would you like to see the original message? Y/N "



