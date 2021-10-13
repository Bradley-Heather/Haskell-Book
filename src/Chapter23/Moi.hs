{-# LANGUAGE InstanceSigs #-}

module Moi where 

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where 
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) = Moi $ \s -> let (a, s1) = g s 
                                 in (f a, s1)

instance Applicative (Moi s) where 
    pure :: a -> Moi s a
    pure a = undefined

    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b 
    Moi f <*> Moi g = undefined

instance Monad (Moi s) where 
    return = pure 

    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b 
    Moi f >>= g = undefined