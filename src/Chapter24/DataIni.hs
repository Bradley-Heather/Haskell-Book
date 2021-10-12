{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Ini where 

import Control.Applicative
import Data.ByteString       (ByteString)
import Data.Char             (isAlpha)
import Data.Map              (Map)
import qualified Data.Map as M
import Data.Text             (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec 
import Text.Trifecta

headerEx :: ByteString
headerEx = "[Blah]"

newtype Header = Header String 
   deriving (Eq, Ord, Show) 

parseBracketPair :: Parser a -> Parser a 
parseBracketPair p = char '[' *> p <* char ']'

parseHeader = parseBracketPair (Header <$> some letter)

assignmentEx :: ByteString
assignmentEx = "woot=1"

type Name  = String 
type Value = String
type Assignments = Map Name Value

parseAssignments :: Parser (Name, Value)
parseAssignments = do 
    name <- some letter 
    _ <- char '=' 
    val <- some (noneOf "\n")
    skipEOL -- important!
    return (name, val)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

commentEx :: ByteString
commentEx = "; last modified 1 April\
            \ 2001 by John Doe"

commentEx' :: ByteString
commentEx' = "; blah\n; woot\n  \n;hah"

skipComments :: Parser ()
skipComments = 
    skipMany (do _ <- char ';' <|> char '#' 
                 skipMany (noneOf "\n")
                 skipEOL)

sectionEx :: ByteString
sectionEx = "; ignore me \n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' = "; comment \n[section]\nhost=wikipedia.org\nalias=Claw\n\n[whatisit]\nred=intoothandclaw"

data Section = Section Header Assignments
   deriving (Eq, Show)

newtype Config = Config (Map Header Assignments)
   deriving (Eq, Show)

skipWhiteSpace :: Parser ()
skipWhiteSpace = skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section 
parseSection = do 
    skipWhiteSpace
    skipComments

    h <- parseHeader 
    skipEOL

    assignments <- some parseAssignments
    return $ Section h (M.fromList assignments) 

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) m = M.insert h a m 

parseIni :: Parser Config 
parseIni = do 
    sections <- some parseSection 
    let mapOfSections = 
             foldr rollup M.empty sections 
    return (Config mapOfSections)