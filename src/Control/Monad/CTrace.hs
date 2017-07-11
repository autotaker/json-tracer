{-# LANGUAGE GeneralizedNewtypeDeriving, FunctionalDependencies, UndecidableInstances #-}
-- |
-- Module      : Control.Monad.CTrace
-- Copyright   : (c) Taku Terao, 2017 
-- License     : BSD3 
-- Maintainer  : autotaker@gmail.com 
-- Stability   : experimental 
-- Portability : GHC
-- Contextual tracing monad, mtl-compatible.

module Control.Monad.CTrace( MonadTrace(..)
                           , TracerT
                           , zoom
                           , runTracerT
                           , noTracerT
                           , ioTracerT) where

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
class Monad m => MonadTrace c m | m -> c where
    -- | Apply the specified function to the tracing context
    update :: (c -> c) -> m ()
    -- | monomorphic version of 'zoom' operation
    zoom'  :: ASetter' c c -> m a -> m a

instance Monad m => MonadTrace c (TracerT c m) where
    update = T.update 
    zoom' = zoom

instance MonadTrace c m => MonadTrace c (ReaderT r m) where
    update = lift . update
    zoom'  = mapReaderT . zoom'

instance (Monoid w, MonadTrace c m) => MonadTrace c (Strict.WriterT w m) where
    update = lift . update
    zoom' = Strict.mapWriterT .zoom'

instance (Monoid w, MonadTrace c m) => MonadTrace c (Lazy.WriterT w m) where
    update = lift . update
    zoom' = Lazy.mapWriterT .zoom'

instance (MonadTrace c m) => MonadTrace c (ExceptT e m) where
    update = lift . update
    zoom' = mapExceptT . zoom'

instance (MonadTrace c m) => MonadTrace c (IdentityT m) where
    update = lift . update
    zoom' = mapIdentityT . zoom'

instance (MonadTrace c m) => MonadTrace c (ListT m) where
    update = lift . update
    zoom' = mapListT . zoom'

instance (MonadTrace c m) => MonadTrace c (MaybeT m) where
    update = lift . update
    zoom' = mapMaybeT . zoom'

instance (MonadTrace c m) => MonadTrace c (Strict.StateT s m) where
    update = lift . update
    zoom' = Strict.mapStateT . zoom'

instance (MonadTrace c m) => MonadTrace c (Lazy.StateT s m) where
    update = lift . update
    zoom' = Lazy.mapStateT . zoom'

instance (Monoid w, MonadTrace c m) => MonadTrace c (Strict.RWST r w s m) where
    update = lift . update
    zoom' = Strict.mapRWST . zoom'

instance (Monoid w, MonadTrace c m) => MonadTrace c (Lazy.RWST r w s m) where
    update = lift . update
    zoom' = Lazy.mapRWST . zoom'

instance (MonadTrace c m) => MonadTrace c (ContT r m) where
    update = lift . update
    zoom' = mapContT . zoom'
