{-# LANGUAGE DataKinds, TypeFamilies, OverloadedLabels #-}
module Fib where
import Lens.Micro
import Control.Monad.CTrace
import Data.PolyDict
import Control.Monad.IO.Class

data Fib
type instance Assoc Fib "arg"  = Int
type instance Assoc Fib "ret"  = Int
type instance Assoc Fib "left"  = Dict Fib
type instance Assoc Fib "right" = Dict Fib

fib :: MonadIO m => Int -> TracerT (Dict Fib) m Int
fib n = do
    update (access #arg ?~ n)
    r <- case n of
        0 -> return 0
        1 -> return 1
        n -> (+) <$> zoom (access' #left  empty) (fib (n-1)) 
                 <*> zoom (access' #right empty) (fib (n-2))
    update (access #ret ?~ r)
    return r


    
