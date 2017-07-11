{-# LANGUAGE GeneralizedNewtypeDeriving, FunctionalDependencies, UndecidableInstances, StandaloneDeriving #-}
-- |
-- Module      : Control.Monad.CTrace
-- Copyright   : (c) Taku Terao, 2017 
-- License     : BSD3 
-- Maintainer  : autotaker@gmail.com 
-- Stability   : experimental 
-- Portability : GHC
-- Contextual tracing monad transformer. transformers-compatible.
module Control.Monad.Trans.CTrace(TracerT, runTracerT, zoom, update, noTracerT, ioTracerT) where

import Control.Monad.Cont.Class
import Control.Monad.Reader
import Control.Monad.Writer.Class
import Control.Monad.Error.Class
import Control.Monad.State.Class
import Control.Monad.RWS.Class
import Lens.Micro
import Data.IORef

-- | Contextual tracing monad transformer type.
--   Tracing context c can be modified through this monad.
newtype TracerT c m a = TracerT (ReaderT (Action c m) m a)
   deriving(Functor,Monad,Applicative, MonadIO, MonadFix)

type Action c m = (c -> c) -> m ()

-- | Perform modification on the tracing context
update :: Monad m => (c -> c) -> TracerT c m ()
update f = TracerT $ ReaderT $ \tracer -> tracer f
{-# INLINE update #-}

-- | Change the tracing context. 
zoom :: ASetter' c c' -> TracerT c' m a -> TracerT c m a
zoom l (TracerT m) = TracerT $ ReaderT $ \action ->
    runReaderT m (\updateFunc -> action (over l updateFunc))
{-# INLINE zoom #-}

instance MonadTrans (TracerT c) where
    lift = TracerT . lift
    {-# INLINE lift #-}

-- | Run the tracer monad with the specified update action.
runTracerT :: ((c -> c) -> m ()) -> TracerT c m a -> m a
runTracerT action (TracerT m) = runReaderT m action
{-# INLINE runTracerT #-}

-- | Run the tracer monad without tracing.
noTracerT :: Monad m => TracerT c m a -> m a
noTracerT = runTracerT (const (return ()))
{-# INLINE noTracerT #-}

-- | Run the tracer monad with update action implemented by 'IORef'
ioTracerT :: MonadIO m => c -> TracerT c m a -> m (a,c)
ioTracerT init m = do
    r <- liftIO $ newIORef init
    v <- runTracerT (liftIO . modifyIORef' r) m
    c <- liftIO $ readIORef r
    return (v,c)
{-# INLINE ioTracerT #-}

instance MonadReader r m => MonadReader r (TracerT c m) where
    ask = lift ask
    reader = lift . reader
    local f (TracerT m) = TracerT (ReaderT $ local f . runReaderT m)

deriving instance MonadWriter w m => MonadWriter w (TracerT c m) 
deriving instance MonadError e m => MonadError e (TracerT c m) 
deriving instance MonadState s m => MonadState s (TracerT c m) 
deriving instance MonadRWS r w s m => MonadRWS r w s (TracerT c m)
deriving instance MonadCont m => MonadCont (TracerT c m)

    
