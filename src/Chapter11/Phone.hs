module Phone where

import Data.Char
import Data.List

-- valid Buttons = "1234567890*#"
type Digit = Char

type Presses = Int

data DaPhone = DaPhone [(Digit, [Char])] 
        deriving Show

keypad :: DaPhone
keypad =  DaPhone [ ('1', "1")
                  , ('2', "2abc")
                  , ('3', "3def")
                  , ('4', "4ghi")
                  , ('5', "5jkl")
                  , ('6', "6mno")
                  , ('7', "7pqrs")
                  , ('8', "8tuv")
                  , ('9', "9wxyz")
                  , ('*', "*^")
                  , ('0', "0+ ")
                  , ('#', "#.,") ]

convo :: [String]
convo = ["Wanna play 20 questions",
         "Ya",
         "U 1st haha",
         "Lol Ok. Have u ever tasted alcohol",
         "Lol ya",
         "Wo ur cool haha. Ur turn",
         "OK. Do u think I am pretty Lol",
         "Lol ya",
         "Just making sure rofl ur turn"]

reverseTaps :: DaPhone -> String -> [(Digit, Presses)]
reverseTaps _ [] = []
reverseTaps keypad (x:xs) 
        | isUpper x  = ('*', 1) : n : reverseTaps keypad xs
        | otherwise  = n : reverseTaps keypad xs
            where n = (,)(fst (findKey keypad y)) (press keypad y)
                  y = toLower x


findKey :: DaPhone -> Char -> (Digit, [Char])
findKey (DaPhone key) x = 
         case find (\(_,o) -> x `elem` o) key of 
               Just i -> i
            
press :: DaPhone -> Char -> Presses
press keypad x = 
         case (\(_,o) -> x `elemIndex` o) $ findKey keypad x of
               Just i -> i 

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd 
