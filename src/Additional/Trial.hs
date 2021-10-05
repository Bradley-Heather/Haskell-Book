module Trial where

data Optional a = Nada | Only a 
   deriving (Eq, Show) 

instance Semigroup a => Semigroup (Optional a) where 
  (<>) Nada Nada = Nada 
  (<>) (Only a) Nada = Only a
  (<>) Nada (Only a) = Only a
  (<>) (Only a) (Only b) = Only (a <> b)

instance Monoid a => Monoid (Optional a) where
  mempty  = Nada  
  mappend = (<>)  
