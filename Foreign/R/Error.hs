{-# LANGUAGE CPP, DeriveDataTypeable #-}
module Foreign.R.Error
  ( RError(..)
  , rThrowErrors
  , catchRErrors
  , rError, rErrorCall
  , rWarning, rWarningCall
  ) where

#include "config.h"

import Prelude hiding (catch)
import Control.Exception
import Data.Typeable
import Foreign
import Foreign.C

import Foreign.R.Types
import Foreign.R.Internals

type R_ErrorHook = R_EXP -> CString -> IO ()
type RErrorHook = REXP -> String -> IO ()

rErrorHook :: RErrorHook -> R_ErrorHook
rErrorHook f s m = do
  s <- extREXP s
  peekCString m >>= f s

foreign import ccall "wrapper" r_ErrorHook :: R_ErrorHook -> IO (FunPtr R_ErrorHook)

rSetErrorHook :: RErrorHook -> IO ()

#ifdef HAVE_R_SETERRORHOOK
foreign import ccall unsafe "R_SetErrorHook" r_SetErrorHook :: FunPtr R_ErrorHook -> IO ()
rSetErrorHook f = r_SetErrorHook =<< r_ErrorHook (rErrorHook f)
#else
rSetErrorHook = return
#endif

data RError = RError { rerrorCall :: Maybe REXP, rerrorMsg :: String }
  deriving (Typeable)

instance Show RError where
  show e = "RError: " ++ rerrorMsg e
instance Exception RError

throwRError :: RErrorHook
throwRError s m = throwIO $ RError (Just s) m

rThrowErrors :: IO ()
rThrowErrors = rSetErrorHook throwRError

foreign import ccall safe "Rf_error" r_Error :: CString -> CString -> IO ()
rError :: String -> IO a
rError m = withCString m (withCString "%s" . r_Error) >> fail "Rf_error return"

foreign import ccall safe "Rf_errorcall" r_ErrorCall :: R_EXP -> CString -> CString -> IO ()
rErrorCall :: REXP -> String -> IO a
rErrorCall s m = withREXP s (\s -> withCString m (withCString "%s" . r_ErrorCall s)) >> fail "Rf_errorcall return"

foreign import ccall safe "Rf_warning" r_Warning :: CString -> CString -> IO ()
rWarning :: String -> IO ()
rWarning m = withCString m $ withCString "%s" . r_Warning

foreign import ccall safe "Rf_warningcall" r_WarningCall :: R_EXP -> CString -> CString -> IO ()
rWarningCall :: REXP -> String -> IO ()
rWarningCall s m = withREXP s $ \s -> withCString m (withCString "%s" . r_WarningCall s)


catchRErrors :: IO a -> IO a
catchRErrors f = catch f $ \(RError c m) ->
  case c of
    Nothing -> rError m
    Just c -> rErrorCall c m
