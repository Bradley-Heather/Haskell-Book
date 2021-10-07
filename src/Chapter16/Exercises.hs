{-# LANGUAGE FlexibleInstances #-}

module Exercises where 

-------------------------
-- | Rearrange 
-- 1. 

data Sum a b = First b | Second a
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First b) = First (f b)
  fmap f (Second a) = Second a

--------------------------
-- 2.

data Company a b c = DeepBlue a b | Something c
  deriving (Eq, Show)

instance Functor (Company a b) where
  fmap f (Something c) = Something (f c)
  fmap _ (DeepBlue a b) = DeepBlue a b

--------------------------
-- 3. 

data More a b = L b a b | R a b a
  deriving (Eq, Show)

instance Functor (More a) where
  fmap f (L a b c) = L (f a) b (f c)
  fmap f (R a b c) = R a (f b) c

--------------------------
-- | Instances
-- 1. 

data Quant a b = Finance | Desk a | Bloor b 
   deriving (Eq, Show)

instance Functor (Quant a) where 
    fmap _ Finance = Finance 
    fmap _ (Desk a) = Desk a 
    fmap f (Bloor b) = Bloor (f b)

--------------------------
-- 2.

data K' a b = K' a 
   deriving (Eq, Show)

instance Functor (K' a) where 
    fmap _ (K' a) = K' a 

---------------------------
-- 3.

newtype Flip f a b = Flip (f b a)
    deriving (Eq, Show)

newtype K a b = K a 
   deriving (Eq, Show)

instance Functor (Flip K a) where 
    fmap f (Flip (K a)) = Flip $ K (f a)

---------------------------
-- 4. 

data EvilGoatConst a b = GoatyConst b 
   deriving (Eq, Show)

instance Functor (EvilGoatConst a) where 
    fmap f (GoatyConst b) = GoatyConst (f b)

---------------------------
-- 5. 

data LiftItOut f a = LiftItOut (f a) 
   deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where 
    fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

----------------------------
-- 6.

data Parappa f g a = DaWrappa (f a) (g a)
   deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where 
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

----------------------------
-- 7. 

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
   deriving (Eq, Show)

instance (Functor g) => Functor (IgnoreOne f g a) where 
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb) 

----------------------------
-- 8. 

data Notorious g o a t = Notorious (g o) (g a) (g t)
   deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where 
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt) 

-----------------------------
-- 9.

data List a = Nil | Cons a (List a)
   deriving (Eq, Show)

instance Functor List where 
    fmap _ Nil         = Nil 
    fmap f (Cons a as) = Cons (f a) (fmap f as)

-----------------------------
-- 10.

data GoatLord a = NoGoat 
                | OneGoat a 
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
    deriving (Eq, Show)

instance Functor GoatLord where 
    fmap _ NoGoat               = NoGoat
    fmap f (OneGoat a)          = OneGoat (f a)
    fmap f (MoreGoats as bs cs) = MoreGoats (fmap f as) (fmap f bs) (fmap f cs)

------------------------------
-- 11.

data TalkToMe a = Halt | Print String a | Read (String -> a)