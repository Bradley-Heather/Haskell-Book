module Parsers where 

import Text.Trifecta 
import Text.Parser.Combinators

stop :: Parser a 
stop = unexpected "stop"

-- read a single char '1'
one = char '1' 

-- read a single char '1' then die
one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop 

oneTwoStop = oneTwo >> eof

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

testParse' :: Parser () -> IO ()
testParse' p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main = do 
    pNL "Stop:"
    testParse stop 

    pNL "one:"
    testParse one 

    pNL"one':"
    testParse one' 

    pNL"oneTwo:"
    testParse oneTwo

    pNL "oneTwoStop:"
    testParse' oneTwoStop

p123 :: String -> IO ()
p123 p = print $ parseString x mempty "123"
            where x = string p 