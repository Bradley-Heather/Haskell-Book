{-# LANGUAGE InstanceSigs #-}

module Types where 


newtype One f a = One (f a)
  deriving (Eq, Show)

instance Functor f => Functor (One f) where
  fmap f (One fa) = One $ fmap f fa
----------------------------

newtype Three f g h a = Three (f (g (h a)))
  deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
  fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha
-----------------------------

newtype Compose f g a = Compose { getCompoose :: f (g a) }
   deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where 
    fmap f (Compose fga) = Compose $ (fmap. fmap) f fga 

instance (Applicative f, Applicative g) => Applicative (Compose f g) where 
    pure :: a -> Compose f g a 
    pure a = Compose $ (pure . pure) a

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b 
    Compose f <*> Compose a = Compose $ (<*>) <$> f <*> a