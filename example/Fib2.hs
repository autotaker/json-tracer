{-# LANGUAGE DataKinds, TypeFamilies, OverloadedLabels, FlexibleContexts #-}
module Fib2 where
import Fib(Fib)
import Lens.Micro
import Control.Monad.CTrace
import Data.PolyDict
import Control.Monad.State.Strict
import qualified Data.IntMap as M

type instance Assoc Fib "calls"  = [(Int,Int)]

memoFib :: (MonadState (M.IntMap Int) m, 
            MonadTrace (Dict Fib) m) => Int -> m Int
memoFib n = do
    memo <- get
    case M.lookup n memo of
        Just v -> return v
        Nothing -> do
            v <- case n of
                0 -> return 0
                1 -> return 1
                _ -> (+) <$> memoFib (n-1) <*> memoFib (n-2)
            modify' (M.insert n v)
            update $ access' #calls [] %~ ((n,v):)
            return v

fib :: Monad m => Int -> TracerT (Dict Fib) m Int
fib n = do
    update (access #arg ?~ n)
    r <- evalStateT (memoFib n) (M.empty)
    update (access #ret ?~ r)
    return r
