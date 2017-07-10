{-# LANGUAGE DataKinds, TypeFamilies, OverloadedLabels #-}
module Main where

import Lib
import PolyDict(Assoc)
import qualified PolyDict as D
import qualified Data.IntMap as M
import Logger
import qualified Sub
import Data.IORef
import Control.Monad
import Lens.Micro
import Lens.Micro.GHC()

data Main
type instance Assoc Main "sub"   = (M.IntMap (D.Dict Sub.Sub))
type instance Assoc Main "count" = Int

doit :: Monad m => LoggerT Main m ()
doit = do
  forM_ [1..10] $ \i -> do
    updateLog (D.access #count . non 0 %~ succ)
    zoom (D.access' #sub M.empty . at i . non D.empty) (Sub.main i)

main :: IO ()
main = do
    r <- newIORef D.empty
    runLoggerT (modifyIORef' r) doit
    readIORef r >>= print
