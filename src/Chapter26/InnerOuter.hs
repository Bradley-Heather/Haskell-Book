module OuterInner where 

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

unwrapMaybe :: ExceptT String (ReaderT () IO) (Maybe Int)
unwrapMaybe = runMaybeT embedded

unwrapExcept :: ReaderT () IO (Either String (Maybe Int))
unwrapExcept = runExceptT unwrapMaybe

unwrapReader :: () -> IO (Either String (Maybe Int))
unwrapReader = runReaderT unwrapExcept


embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded' = MaybeT . ExceptT . ReaderT $ const $ return $ Right $ Just 1