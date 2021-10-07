module Chapter20 where 

------------------------

data Identity a = Identity a 

instance Foldable Identity where 
    foldr f z (Identity x) = f x z

    foldl f z (Identity x) = f z x 

    foldMap f (Identity x) = f x 

--------------------------

data Optional a = Nada | Yep a 
    deriving (Show, Eq) 

instance Foldable Optional where 
    foldr _ z Nada    = z 
    foldr f z (Yep x) = f x z

    foldl _ z Nada    = z 
    foldl f z (Yep x) = f z x

    foldMap f Nada    = mempty 
    foldMap f (Yep x) = f x 

--------------------
-- 1.

data Constant a b = Constant b 
   deriving (Show, Eq)

instance Foldable (Constant a) where 
    foldMap f (Constant b) = f b

--------------------
-- 2.

data Two a b = Two a b
   deriving (Show, Eq)

instance Foldable (Two a) where 
    foldMap f (Two a b) = f b 

---------------------
-- 3.

data Three a b c = Three a b c 
   deriving (Show, Eq)

instance Foldable (Three a b) where 
    foldMap f (Three a b c) = f c

---------------------
-- 4.

data Three' a b = Three' a b b
   deriving (Show, Eq)

instance Foldable (Three' a) where 
    foldMap f (Three' a b c) = f b <> f c 

---------------------- 
-- 5.

data Four a b = Four a b b b
   deriving (Show, Eq)

instance Foldable (Four a) where 
     foldMap f (Four a b c d) = f b <> f c <> f d 

------------------------

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a 
filterF f = foldMap (\x -> if f x then pure x else mempty)