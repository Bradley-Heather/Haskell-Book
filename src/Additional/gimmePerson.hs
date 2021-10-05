module Gimme where 

type Name = String 
type Age = Integer 

data Person = Person Name Age 
   deriving Show 

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String 
   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person 
mkPerson name age 
   | name /= "" && age > 0 = Right $ Person name age 
   | name == "" = Left NameEmpty 
   | not (age > 0) = Left AgeTooLow 
   | otherwise = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age 

gimmePerson :: IO (Either PersonInvalid Person) 
gimmePerson = do 
  putStr "Please input your name: "
  n <- getLine
  putStr "Pleas input your age: "
  a <- getLine
  case mkPerson n (read a) of
    (Right _ ) -> do 
      putStr "Yay! successfully got a person: "
      return (mkPerson n (read a)) 
    (Left _) -> do 
      putStr "Error Occured: " 
      return (mkPerson n (read a)) 
              




