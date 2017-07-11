
{-|
Module      : JSON.Trace
Copyright   : (c) Taku Terao, 2017 
License     : BSD3 
Maintainer  : autotaker@gmail.com 
Stability  : experimental 
Portability : GHC
type-safe json-structured tracing

-}

module JSON.Trace( dictTraceT 
                 , module Data.PolyDict 
                 , module Control.Monad.CTrace 
                 )where
   
import Data.PolyDict    
import Control.Monad.Trans
import Control.Monad.CTrace

-- | run the tracer monad whose tracing context is 'Dict'
dictTraceT :: (MonadIO m) => TracerT (Dict c) m a -> m (a, Dict c)
dictTraceT = ioTracerT empty

