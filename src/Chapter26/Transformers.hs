{-# LANGUAGE InstanceSigs #-}

module Transformers where 

-- MaybeT

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where 
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where 
    pure a = MaybeT $ (pure . pure) a 

    MaybeT f <*> MaybeT ma = MaybeT $ (<*>) <$> f <*> ma 

instance Monad m => Monad (MaybeT m) where 
    return = pure 

    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b 
    MaybeT ma >>= f = 
        MaybeT $ do
            v <- ma 
            case v of 
                Nothing -> pure Nothing
                Just y  -> runMaybeT (f y) 

--------------------------------------
-- EitherT 

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where 
    fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where 
    pure a = EitherT $ (pure . pure) a

    EitherT meab <*> EitherT mea = EitherT $ (<*>) <$> meab <*> mea

instance Monad m => Monad (EitherT e m) where 
    return = pure 

    EitherT mea >>= f =
        EitherT $ do 
            v <- mea 
            case v of 
                Left e  -> return $ Left e 
                Right a -> runEitherT (f a) 

swapEither :: Either e a -> Either a e 
swapEither = undefined

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e 
swapEitherT = undefined 

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c 
eitherT = undefined

--------------------------------------
-- ReaderT

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where 
    fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where 
    pure a = ReaderT $ pure (pure a)

    ReaderT rmab <*> ReaderT rma = ReaderT $ (<*>) <$> rmab <*> rma 

instance Monad m => Monad (ReaderT r m) where 
    return = pure 

    ReaderT rma >>= f = 
        ReaderT $ \r -> do
            a <- rma r 
            runReaderT (f a) r 

---------------------------------------
-- StateT 

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance Functor m => Functor (StateT s m) where 
    fmap f (StateT sma) = StateT $ \s -> (\ (a, s1) -> (f a, s1)) <$> sma s 

instance (Monad m) => Applicative (StateT s m) where 
    pure a = StateT $ \s -> pure (a, s) 

    (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
    StateT smab <*> StateT sma = 
        StateT $ \s -> do
            (ab, s1) <- smab s
            fmap (\ (a, s2) -> (ab a, s2)) $ sma s1
                                 
instance (Monad m) => Monad (StateT s m) where 
    return = pure 

    StateT sma >>= f = 
        StateT $ \s -> do  
            (a, s1) <- sma s 
            runStateT (f a) s1


