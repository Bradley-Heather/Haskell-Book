module Programmer where

import Data.List

data OperatingSystem = GnuPlusLinux 
                    | OpenBSDPlusNevermindJustBSDSTill
                    | Mac
                    | Windows
            deriving (Eq, Show)

data ProgLang = Haskell 
             | Agda 
             | Idris 
             | PureScript
        deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem
                             , lang :: ProgLang 
                             }
                deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac 
                        , lang = Haskell
                        }

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux
                      , OpenBSDPlusNevermindJustBSDSTill
                      , Mac
                      , Windows 
                      ]

allLanguages :: [ProgLang]
allLanguages = [ Haskell 
               , Agda
               , Idris
               , PureScript
               ]

allProgrammers :: [Programmer]
allProgrammers = nub [Programmer x y | x <- allOperatingSystems, y <- allLanguages]