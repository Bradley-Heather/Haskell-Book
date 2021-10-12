class BiFunctor p where
    {-# MINIMAL bimap | first, second #-}

    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g 

    first :: (a -> b) -> p a c -> p b c 
    first f = bimap f id 

    second :: (b -> c) -> p a b -> p a c 
    second = bimap id 

data Deux a b = Deux a b 
   deriving (Eq, Show)

instance BiFunctor Deux where 
   bimap f g (Deux a b) = Deux (f a) (g b)

newtype Const a b = Const a 
   deriving (Eq, Show)  

instance BiFunctor Const where 
    bimap f g (Const a) = Const $ f a

data Drei a b c = Drei a b c 
   deriving (Eq, Show)

instance BiFunctor (Drei a) where 
    bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b 
   deriving (Eq, Show)

instance BiFunctor (SuperDrei a) where 
    bimap f g (SuperDrei a b) = SuperDrei a (f b)

newtype SemiDrei a b c = SemiDrei a 
   deriving (Eq, Show)

instance BiFunctor (SemiDrei a) where 
    bimap f g (SemiDrei a) = SemiDrei a 

data Quadriceps a b c d = Quadzzz a b c d 
   deriving (Eq, Show)

instance BiFunctor (Quadriceps a b) where
    bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

data Either' a b = Left' a | Right' b 
   deriving (Eq, Show)

instance BiFunctor Either' where 
    bimap f _ (Left' a)  = Left' $ f a
    bimap _ g (Right' b) = Right' $ g b