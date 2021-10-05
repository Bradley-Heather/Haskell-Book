module CeasarCipher where 

import Data.Char (ord, chr)

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