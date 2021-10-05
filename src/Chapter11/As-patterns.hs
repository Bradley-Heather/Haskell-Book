module AsPatterns where

import Data.Char (toUpper)

f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do 
    print a 
    return t

doubleUp :: [a] -> [a]
doubleUp  [] = []
doubleUp xs@(x: _) = x : xs

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _  = True
isSubseqOf _  [] = False
isSubseqOf xs@(x:xt) (y:yt)
  | x == y     = isSubseqOf xt yt
  | otherwise  = isSubseqOf xs yt

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = map capatilize (words s)
      where capatilize n@(x:xs) = (n , toUpper x : xs)

capitalizeWord :: String -> String 
capitalizeWord (x:a:xs) 
        | x == ' '  = x : toUpper a : xs
        | otherwise = toUpper x : (a:xs)  

capatalizeParagraph :: String -> String
capatalizeParagraph n = concatMap capitalizeWord $ splitOn '.' n 

splitOn :: Char -> String -> [String]
splitOn n = foldr go [""]
  where go c xt@(x:xs)
          | c == n  = [c]:xt
          | otherwise = (c:x):xs
