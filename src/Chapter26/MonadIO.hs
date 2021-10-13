module MonadIO where 

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Class

class Monad m => MonadIO m where
  liftIO :: IO a -> m a

-- 1. IdentityT

instance (MonadIO m) => MonadIO (IdentityT m) where 
    liftIO = IdentityT . liftIO 

-- 2. ExceptT

instance  (MonadIO m) => MonadIO (ExceptT e m) where 
    liftIO = lift . liftIO

-- 3. MaybeT 

instance (MonadIO m) => MonadIO (MaybeT m) where 
    liftIO = lift . liftIO

-- 4. ReaderT

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

-- 5. ReaderT 

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO