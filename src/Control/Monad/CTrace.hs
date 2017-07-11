{-# LANGUAGE GeneralizedNewtypeDeriving, FunctionalDependencies, UndecidableInstances #-}
-- |
-- Module      : Control.Monad.CTrace
-- Copyright   : (c) Taku Terao, 2017 
-- License     : BSD3 
-- Maintainer  : autotaker@gmail.com 
-- Stability   : experimental 
-- Portability : GHC
-- Contextual tracing monad, mtl-compatible.

module Control.Monad.CTrace(MonadTracer(..)
                           ,TracerT
                           ,zoom
                           ,runTracerT) where

import Control.Monad.Trans.CTrace hiding (update)
import qualified Control.Monad.Trans.CTrace as T(update)
import Lens.Micro

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Cont
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.Class(lift)

-- | contextual tracing monad
class Monad m => MonadTracer c m | m -> c where
    -- | Apply the specified function to the tracing context
    update :: (c -> c) -> m ()
    -- | monomorphic version of 'zoom' operation
    zoom'  :: ASetter' c c -> m a -> m a


instance Monad m => MonadTracer c (TracerT c m) where
    update = T.update 
    zoom' = zoom

instance MonadTracer c m => MonadTracer c (ReaderT r m) where
    update = lift . update
    zoom'  = mapReaderT . zoom'

instance (Monoid w, MonadTracer c m) => MonadTracer c (Strict.WriterT w m) where
    update = lift . update
    zoom' = Strict.mapWriterT .zoom'

instance (Monoid w, MonadTracer c m) => MonadTracer c (Lazy.WriterT w m) where
    update = lift . update
    zoom' = Lazy.mapWriterT .zoom'

instance (MonadTracer c m) => MonadTracer c (ExceptT e m) where
    update = lift . update
    zoom' = mapExceptT . zoom'

instance (MonadTracer c m) => MonadTracer c (IdentityT m) where
    update = lift . update
    zoom' = mapIdentityT . zoom'

instance (MonadTracer c m) => MonadTracer c (ListT m) where
    update = lift . update
    zoom' = mapListT . zoom'

instance (MonadTracer c m) => MonadTracer c (MaybeT m) where
    update = lift . update
    zoom' = mapMaybeT . zoom'

instance (MonadTracer c m) => MonadTracer c (Strict.StateT s m) where
    update = lift . update
    zoom' = Strict.mapStateT . zoom'

instance (MonadTracer c m) => MonadTracer c (Lazy.StateT s m) where
    update = lift . update
    zoom' = Lazy.mapStateT . zoom'

instance (Monoid w, MonadTracer c m) => MonadTracer c (Strict.RWST r w s m) where
    update = lift . update
    zoom' = Strict.mapRWST . zoom'

instance (Monoid w, MonadTracer c m) => MonadTracer c (Lazy.RWST r w s m) where
    update = lift . update
    zoom' = Lazy.mapRWST . zoom'

instance (MonadTracer c m) => MonadTracer c (ContT r m) where
    update = lift . update
    zoom' = mapContT . zoom'
