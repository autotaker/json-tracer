{-# LANGUAGE DataKinds, TypeFamilies, OverloadedLabels #-}
module Sub where

import PolyDict(Assoc)
import qualified PolyDict as D
import Control.Monad
import Logger
import Lens.Micro

data Sub
type instance Assoc Sub "count" = String 
type instance Assoc Sub "arg" = Int
type instance Assoc Sub "rec" = D.Dict Sub

sub :: Monad m => Int -> LoggerT Sub m ()
sub 0 = return ()
sub i = do
    zoom' (D.access' #rec D.empty) (sub (i-1))
    updateLog $ D.access #arg ?~ i

main :: Monad m => Int -> LoggerT Sub m ()
main n = do
    sub n
    forM_ [1..n] $ \i ->
        updateLog  (D.access' #count "" %~ (\s -> show i ++ " " ++ s))


