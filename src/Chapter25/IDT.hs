module IDT where 

newtype IdentityT f a = IdentityT { runIdentityT :: f a }
   deriving (Eq, Show)

instance Functor f => Functor (IdentityT f) where 
    fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative f => Applicative (IdentityT f) where 
    pure a = IdentityT $ pure a

    IdentityT fab <*> IdentityT fa = IdentityT $ fab <*> fa 

instance Monad m => Monad (IdentityT m) where 
    return = pure

    IdentityT ma >>= f = IdentityT $ ma >>= runIdentityT . f 