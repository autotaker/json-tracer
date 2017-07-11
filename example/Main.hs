{-# LANGUAGE DataKinds, TypeFamilies, OverloadedLabels #-}
import Control.Monad.CTrace
import Data.PolyDict
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as B
import Data.Time(NominalDiffTime, getCurrentTime, diffUTCTime)
import Control.Monad.Trans
import Control.Monad
import Lens.Micro
import qualified Fib
import qualified Fib2

data Main
type instance Assoc Main "elapsed_time" = NominalDiffTime
type instance Assoc Main "tag" = String
type instance Assoc Main "fib" = (Dict Fib.Fib)

doit1 :: TracerT (Dict Main) IO Int
doit1 = do
  update $ access #tag ?~ "doit1"
  t_start <- liftIO getCurrentTime
  r <- zoom (access' #fib empty) (Fib.fib 5)
  t_end <- liftIO getCurrentTime
  update $ access #elapsed_time ?~ (diffUTCTime t_end t_start)
  return r

doit2 :: TracerT (Dict Main) IO Int
doit2 = do
  update $ access #tag ?~ "doit2"
  t_start <- liftIO getCurrentTime
  r <- zoom (access' #fib empty) (Fib2.fib 5)
  t_end <- liftIO getCurrentTime
  update $ access #elapsed_time ?~ (diffUTCTime t_end t_start)
  return r


main :: IO ()
main = do
    forM_ [doit1,doit2] $ \doit -> do
        (r, d) <- ioTracerT empty doit
        B.putStr (encodePretty d)
        putStrLn ""
        print r
    
