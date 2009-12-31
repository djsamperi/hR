{-|
  Interface to R embedded library.
-}
module Foreign.R.Embedded
  ( initialize
  , end
  ) where

import Control.Monad
import Foreign
import Foreign.C
import System.Posix.Env

import Foreign.R.Config
import Foreign.R.Util
import Foreign.R.Error

foreign import ccall safe "Rf_initEmbeddedR" rf_initEmbeddedR :: CInt -> Ptr CString -> IO CInt
foreign import ccall safe "Rf_endEmbeddedR"  rf_endEmbeddedR  :: CInt -> IO ()

-- |Initializes the R library and environment, passing the specified strings as command name and arguments.
-- 'end' must be called when finished.
initialize :: Maybe (String, [String]) -> IO ()
initialize Nothing = initialize (Just ("HR", ["--gui=none", "--silent"]))
initialize (Just (arg0,args)) = do
  setEnv "R_HOME" rHome False
  r <- withMany withCString (arg0:args) $ \a -> withArrayLen a (rf_initEmbeddedR . ii)
  unless (r > 0) $ fail ("Rf_initEmbeddedR: " ++ show r)
  rThrowErrors

-- |Ends the embedded R session.
end :: IO ()
end = rf_endEmbeddedR 0
