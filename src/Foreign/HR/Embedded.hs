{-|
  Interface to R embedded library.
-}
module Foreign.HR.Embedded
  ( initialize
  , end
  ) where

import Control.Monad
import Foreign
import Foreign.C

-- import System.Posix.Env
import System.Environment(setEnv)
import System.Process(readProcess)

import Foreign.HR.Util
import Foreign.HR.Error

foreign import ccall safe "Rf_initEmbeddedR" rf_initEmbeddedR :: CInt -> Ptr CString -> IO CInt
foreign import ccall safe "Rf_endEmbeddedR"  rf_endEmbeddedR  :: CInt -> IO ()

-- |Initializes the R library and environment, passing the specified strings as command name and arguments.
-- 'end' must be called when finished.
initialize :: Maybe (String, [String]) -> IO ()
initialize Nothing = initialize (Just ("HR", ["--gui=none", "--silent", "--vanilla"]))
initialize (Just (arg0,args)) = do
  setEnv "R_HOME" =<< fmap (head . lines) (readProcess "R" ["-e",
                                                            "cat(R.home())",
                                                            "--quiet",
                                                            "--slave"] "")
                                                            
  r <- withMany withCString (arg0:args) $ \a -> withArrayLen a (rf_initEmbeddedR . ii)
  unless (r > 0) $ fail ("Rf_initEmbeddedR: " ++ show r)
  rThrowErrors

-- |Ends the embedded R session.
end :: IO ()
end = rf_endEmbeddedR 0
