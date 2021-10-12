module Text.Fractions where 

import Control.Applicative 
import Data.Ratio ((%))
import Text.Trifecta 

badFraction    = "1/0"
alsoBad        = "10"
shouldWork     = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational 
parseFraction = do 
    numerator <- decimal 
    char '/' 
    denominator <- decimal
    case denominator of
       0 -> fail "Denominator is Zero"
       _ -> return (numerator % denominator)

-------------------------------
-- Try Try 

type FractionOrDecimal = Either Rational Double

parseNos :: Parser FractionOrDecimal
parseNos = (Left <$> parseFraction) <|> (Right <$> try double)






main :: IO ()
main = do 
    let parseFraction' = parseString parseFraction mempty 

    print $ parseFraction' shouldWork
    print $ parseFraction' shouldAlsoWork
    print $ parseFraction' alsoBad
    print $ parseFraction' badFraction

pInt :: Parser Integer 
pInt = do 
    a <- integer 
    _ <- eof 
    return a 