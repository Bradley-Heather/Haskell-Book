
{-# LANGUAGE FlexibleInstances #-}
module Functor where


-- import Test.QuickCheck

import qualified Distribution.Types.Benchmark as Rearrange
import Language.Haskell.TH.Syntax (Lift)
replaceWithP :: b -> Char 
replaceWithP = const 'P'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe [Char]] -> Char 
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char 
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP 

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

main :: IO ()
main = do 
    putStr  "replaceWithP' lms:    "
    print (replaceWithP' lms)

    putStr  "liftedReplace lms:    "
    print (liftedReplace lms)

    putStr  "liftedReplace' lms:    "
    print (liftedReplace' lms)

    putStr  "twiceLifted lms:    "
    print (twiceLifted lms)

    putStr  "twiceLifted' lms:    "
    print (twiceLifted' lms)

    putStr  "thriceLifted lms:    "
    print (thriceLifted lms)

    putStr  "thriceLifted' lms:    "
    print (thriceLifted' lms)

------------------------------------

e :: IO Integer 
e = let ioi = readIO "1" :: IO Integer 
        changed = fmap (read . ("123"++) . show) ioi 
    in fmap (*3) changed

------------------------------------

-- Instances of Func

newtype Identity a = Identity a
   deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

-----------------------------------

data Pair a = Pair a a 
    deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

------------------------------------

data Two a b = Two a b 
    deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

-------------------------------------

data Three a b c = Three a b c 
   deriving (Eq, Show)

instance Functor (Three a b) where 
    fmap f (Three a b c) = Three a b (f c)

-------------------------------------

data Three' a b = Three' a b b 
   deriving (Eq, Show)

instance Functor (Three' a) where 
    fmap f (Three' a b c) = Three' a (f b) (f c)

--------------------------------------

data Four a b c d = Four a b c d 
   deriving (Eq, Show) 

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

--------------------------------------

data Four' a b = Four' a a a b 
   deriving (Eq, Show)

instance Functor (Four' a) where 
     fmap f (Four' a b c d) = Four' a b c (f d)

data Four'' a b = Four'' a b b b 
   deriving (Eq, Show)

instance Functor (Four'' a) where
    fmap f (Four'' a b c d) = Four'' a (f b) (f c) (f d)

---------------------------------------

-- QuickCheck 

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool 
functorIdentity f = fmap id f == f 

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool 
functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

----------------------------------------

data Possibly a = LolNope | Yeppers a 
   deriving (Eq, Show)

instance Functor Possibly where 
    fmap f (Yeppers a) = Yeppers (f a) 
    fmap _ LolNope = LolNope 

-----------------------------------------

data Sum a b = First a | Second b 
   deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (First a)  = First a
    fmap f (Second b) = Second (f b)

------------------------------------------
-- Rearrange...

-- 1)
data Sum' b a = First' a | Second' b 

instance Functor (Sum' b) where 
    fmap f (First' a)  = First' (f a)
    fmap f (Second' b) = Second' b

-- 2) 
data Company a b c = DeepBlue a b | Something c 

instance Functor (Company e e') where 
    fmap f (Something b)  = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c 

-- 3)
data More b a = L a b a | R b a b 
   deriving (Eq, Show)

instance Functor (More x) where 
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b' 

-----------------------------------------------
-- Functor Instances

-- 1)
data Quant a b = Finance | Desk a | Bloor b 
   deriving (Eq, Show)

instance Functor (Quant a) where 
    fmap f Finance   = Finance 
    fmap f (Desk a)  = Desk a
    fmap f (Bloor b) = Bloor (f b)

-- 2)
data Kap a b = Kap a

instance Functor (Kap a) where 
    fmap f (Kap a) = Kap a

-- 3)
newtype Flip f a b = Flip (f b a)
   deriving (Eq, Show)

newtype K a b = K a 
   deriving (Eq, Show)

instance Functor (Flip K a) where 
    fmap f (Flip (K a)) = Flip $ K (f a)

-- 4)
data EvilGoateeConst a b = GoatyConst b  

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

-- 5)
data LiftOut f a = Liftout (f a)

instance Functor f => Functor (LiftOut f) where
    fmap f (Liftout fa) = Liftout (fmap f fa)

-- 6)
data Parappa f g a = DaWrappa (f a) (g a) 

instance (Functor f, Functor g) => Functor (Parappa f g) where 
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- 7) 
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where 
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

-- 8)
data Notorious g o a t = Notorious (g o) (g a) (g t) 

instance (Functor g) => Functor (Notorious g o a)  where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt) 

-- 9)
data List a = Nil | Cons a (List a) 

instance Functor List where 
    fmap _ Nil               = Nil
    fmap f (Cons a x) = Cons (f a) (fmap f x)
                 

-- To Do 
{- 
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a)
                                                 (GoatLord a)
                                                 (GoatLord a)
                                                 -}

-- data TalkToMe a = Halt |Print String a | Read (String -> a)

