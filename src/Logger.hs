{-# LANGUAGE GeneralizedNewtypeDeriving, FunctionalDependencies #-}
module Logger(LogAction, LoggerT, runLoggerT, zoom, MonadLogger(..)) where

import PolyDict(Dict)
import Control.Monad.Reader
import Lens.Micro

class Monad m => MonadLogger r m | m -> r where
    updateLog :: (Dict r -> Dict r) -> m ()
    zoom' :: ASetter' (Dict r) (Dict r) -> m a -> m a

instance Monad m => MonadLogger r (LoggerT r m) where
    updateLog f = LoggerT $ do
        logger <- ask
        lift (logger f)
    {-# INLINE updateLog #-}
    zoom' l = zoom l
    {-# INLINE zoom' #-}


newtype LoggerT r m a = LoggerT (ReaderT (LogAction r m) m a)
   deriving(Functor,Monad,Applicative, MonadIO, MonadFix)

type LogAction r m = (Dict r -> Dict r) -> m ()

zoom :: ASetter' (Dict r) (Dict r') -> LoggerT r' m a -> LoggerT r m a
zoom l (LoggerT m) = LoggerT $ ReaderT $ \logAction ->
    runReaderT m (\updateFunc -> logAction (over l updateFunc))

instance MonadTrans (LoggerT r) where
    lift = LoggerT . lift
    {-# INLINE lift #-}

runLoggerT :: LogAction r m -> LoggerT r m a -> m a
runLoggerT logAction (LoggerT m) = runReaderT m logAction

