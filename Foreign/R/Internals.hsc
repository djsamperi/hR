{-# LANGUAGE TypeSynonymInstances #-}
module Foreign.R.Internals
  ( withREXP
  , newREXP, extREXP
  , rNilValue
  , rMissingArg, rUnboundValue
  , rNaString, rBlankString
  , rBracket2Symbol, rBracketSymbol, rBraceSymbol, rClassSymbol, rDimNamesSymbol, rDimSymbol, rDollarSymbol, rDotsSymbol, rDropSymbol, rLevelsSymbol, rModeSymbol, rNamesSymbol, rRowNamesSymbol, rSeedsSymbol, rTspSymbol
  , rTYPEOF
  , rAllocREXP
  , rATTRIB, rSET_ATTRIB
  , rGetAttrib, rSetAttrib
  , rLENGTH
  , rAllocVector
  , rPRINTNAME
  , rInstall, rInstallChar
  , rCHAR
  , rMkChar
  , rLOGICAL, rINTEGER
  , rREAL, rCOMPLEX
  , rComplexPeek, rComplexPoke
  , rRAW
  , rSTRING_PTR
  , rSTRING_ELT, rSET_STRING_ELT
  , rVECTOR_PTR
  , rVECTOR_ELT, rSET_VECTOR_ELT
  , rTAG, rCAR, rCDR
  , rCons
  , rSET_TAG, rSETCAR, rSETCDR
  , rLCons
  , rFRAME, rENCLOS
  , rFORMALS, rBODY, rCLOENV
  , rSET_FORMALS, rSET_BODY, rSET_CLOENV
  , rGlobalEnv, rEmptyEnv, rBaseEnv
  , rEval, rTryEval
  ) where

import Control.Monad
import Foreign
import Foreign.C

import Foreign.R.Util
import Foreign.R.Types

#include <Rinternals.h>

instance Enum SEXPTYPE where
  toEnum (#const NILSXP) = NILSXP
  toEnum (#const SYMSXP) = SYMSXP
  toEnum (#const LISTSXP) = LISTSXP
  toEnum (#const CLOSXP) = CLOSXP
  toEnum (#const ENVSXP) = ENVSXP
  toEnum (#const PROMSXP) = PROMSXP
  toEnum (#const LANGSXP) = LANGSXP
  toEnum (#const SPECIALSXP) = SPECIALSXP
  toEnum (#const BUILTINSXP) = BUILTINSXP
  toEnum (#const CHARSXP) = CHARSXP
  toEnum (#const LGLSXP) = LGLSXP
  toEnum (#const INTSXP) = INTSXP
  toEnum (#const REALSXP) = REALSXP
  toEnum (#const CPLXSXP) = CPLXSXP
  toEnum (#const STRSXP) = STRSXP
  toEnum (#const DOTSXP) = DOTSXP
  toEnum (#const ANYSXP) = ANYSXP
  toEnum (#const VECSXP) = VECSXP
  toEnum (#const EXPRSXP) = EXPRSXP
  toEnum (#const BCODESXP) = BCODESXP
  toEnum (#const EXTPTRSXP) = EXTPTRSXP
  toEnum (#const WEAKREFSXP) = WEAKREFSXP
  toEnum (#const RAWSXP) = RAWSXP
  toEnum (#const S4SXP) = S4SXP
  toEnum (#const FUNSXP) = FUNSXP
  fromEnum NILSXP = #const NILSXP
  fromEnum SYMSXP = #const SYMSXP
  fromEnum LISTSXP = #const LISTSXP
  fromEnum CLOSXP = #const CLOSXP
  fromEnum ENVSXP = #const ENVSXP
  fromEnum PROMSXP = #const PROMSXP
  fromEnum LANGSXP = #const LANGSXP
  fromEnum SPECIALSXP = #const SPECIALSXP
  fromEnum BUILTINSXP = #const BUILTINSXP
  fromEnum CHARSXP = #const CHARSXP
  fromEnum LGLSXP = #const LGLSXP
  fromEnum INTSXP = #const INTSXP
  fromEnum REALSXP = #const REALSXP
  fromEnum CPLXSXP = #const CPLXSXP
  fromEnum STRSXP = #const STRSXP
  fromEnum DOTSXP = #const DOTSXP
  fromEnum ANYSXP = #const ANYSXP
  fromEnum VECSXP = #const VECSXP
  fromEnum EXPRSXP = #const EXPRSXP
  fromEnum BCODESXP = #const BCODESXP
  fromEnum EXTPTRSXP = #const EXTPTRSXP
  fromEnum WEAKREFSXP = #const WEAKREFSXP
  fromEnum RAWSXP = #const RAWSXP
  fromEnum S4SXP = #const S4SXP
  fromEnum FUNSXP = #const FUNSXP

withREXP :: REXP -> (R_EXP -> IO a) -> IO a
withREXP = withForeignPtr

extREXP :: R_EXP -> IO REXP
extREXP = newForeignPtr_

#let extsexp f = "\
foreign import ccall unsafe \"&R_%1$s\" r_%1$s_ptr :: Ptr R_EXP\n\
r_%1$s :: IO R_EXP\n\
r_%1$s = peek r_%1$s_ptr\n\
r%1$s :: IO REXP\n\
r%1$s = extREXP =<< r_%1$s\n\
", #f
--"

#extsexp NilValue
#extsexp UnboundValue
#extsexp MissingArg
#extsexp GlobalEnv
#extsexp EmptyEnv
#extsexp BaseEnv
#extsexp Bracket2Symbol
#extsexp BracketSymbol
#extsexp BraceSymbol
#extsexp ClassSymbol
#extsexp DimNamesSymbol
#extsexp DimSymbol
#extsexp DollarSymbol
#extsexp DotsSymbol
#extsexp DropSymbol
#extsexp LevelsSymbol
#extsexp ModeSymbol
#extsexp NamesSymbol
#extsexp RowNamesSymbol
#extsexp SeedsSymbol
#extsexp TspSymbol
#extsexp NaString
#extsexp BlankString

r_Specials :: IO [R_EXP]
r_Specials = sequence [r_NilValue, r_UnboundValue, r_MissingArg, r_GlobalEnv, r_EmptyEnv, r_BaseEnv, r_NaString, r_BlankString]

foreign import ccall unsafe "R_PreserveObject" r_PreserveObject :: R_EXP -> IO ()
foreign import ccall unsafe "&R_ReleaseObject" r_ReleaseObject_ptr :: FunPtr (R_EXP -> IO ())

newREXP :: R_EXP -> IO REXP
newREXP s 
  | s == nullPtr = rNilValue
  | otherwise = do
    spec <- r_Specials
    t <- r_TYPEOF s
    if t == ii (fromEnum SYMSXP) || s `elem` spec {- symbols live forever? -}
      then extREXP s
      else do
	r_PreserveObject s
	newForeignPtr r_ReleaseObject_ptr s

instance Storable REXP where
  sizeOf = sizeOf . unsafeForeignPtrToPtr 
  alignment = alignment . unsafeForeignPtrToPtr 
  peek = newREXP <=< peek . castPtr
  poke p v = withREXP v $ poke (castPtr p)

#let sexpget f, ct, t = "\
foreign import ccall unsafe \"%1$s\" r_%1$s :: R_EXP -> IO %2$s\n\
r%1$s :: REXP -> IO %3$s\n\
r%1$s s = withREXP s $ r_%1$s >=>\
", #f, #ct, #t
#let sexpfun f = " newREXP", ({ hsc_sexpget(f, R_EXP, REXP) })
#let sexpset f = "\
foreign import ccall safe \"%1$s\" r_%1$s :: R_EXP -> R_EXP -> IO ()\n\
r%1$s :: REXP -> REXP -> IO ()\n\
r%1$s s v = withREXP s $ withREXP v . r_%1$s\
", "SET" #f
--"

#sexpget TYPEOF, CInt, SEXPTYPE
  return . toEnum . ii

foreign import ccall safe "Rf_allocSExp" r_AllocSExp :: (#type SEXPTYPE) -> IO R_EXP
rAllocREXP :: SEXPTYPE -> IO REXP
rAllocREXP t = r_AllocSExp (ii $ fromEnum t) >>= newREXP

#sexpfun ATTRIB
#sexpset _ATTRIB

foreign import ccall safe "Rf_getAttrib" r_GetAttrib :: R_EXP -> R_EXP -> IO R_EXP
rGetAttrib :: REXP -> REXP -> IO REXP
rGetAttrib s a = withREXP s $ \s -> withREXP a $ r_GetAttrib s >=> newREXP

foreign import ccall safe "Rf_setAttrib" r_SetAttrib :: R_EXP -> R_EXP -> R_EXP -> IO R_EXP
rSetAttrib :: REXP -> REXP -> REXP -> IO ()
rSetAttrib s a v = void $ withREXP s $ \s -> withREXP a $ withREXP v . r_SetAttrib s

#sexpget OBJECT, CInt, Bool
  return . toBool

#sexpget LENGTH, CInt, Int
  return . ii

foreign import ccall safe "Rf_allocVector" r_AllocVector :: (#type SEXPTYPE) -> (#type R_len_t) -> IO R_EXP
rAllocVector :: SEXPTYPE -> Int -> IO REXP
rAllocVector t i = r_AllocVector (ii $ fromEnum t) (ii i) >>= newREXP

#sexpfun PRINTNAME

foreign import ccall safe "Rf_install" r_Install :: CString -> IO R_EXP
rInstall :: String -> IO REXP
rInstall s = withCString s r_Install >>= extREXP

foreign import ccall safe "R_CHAR" r_CHAR :: R_EXP -> IO CString
rCHAR :: REXP -> IO (Ptr CChar)
rCHAR s = withREXP s r_CHAR

rInstallChar :: REXP -> IO REXP
rInstallChar s = withREXP s $ r_CHAR >=> r_Install >=> extREXP

foreign import ccall safe "Rf_mkCharLen" r_MkCharLen :: CString -> CInt -> IO R_EXP
rMkChar :: String -> IO REXP
rMkChar s = withCStringLen s $ \(s,l) -> r_MkCharLen s (ii l) >>= newREXP

type R_Logical = CInt

foreign import ccall safe "LOGICAL" r_LOGICAL :: R_EXP -> IO (Ptr CInt)
rLOGICAL :: REXP -> IO (Ptr CInt)
rLOGICAL s = withREXP s r_LOGICAL

foreign import ccall safe "INTEGER" r_INTEGER :: R_EXP -> IO (Ptr CInt)
rINTEGER :: REXP -> IO (Ptr CInt)
rINTEGER s = withREXP s r_INTEGER

foreign import ccall safe "RAW" r_RAW :: R_EXP -> IO (Ptr (#type Rbyte))
rRAW :: REXP -> IO (Ptr Word8)
rRAW s = withREXP s r_RAW

foreign import ccall safe "REAL" r_REAL :: R_EXP -> IO (Ptr CDouble)
rREAL :: REXP -> IO (Ptr CDouble)
rREAL s = withREXP s r_REAL

data R_complex

foreign import ccall safe "COMPLEX" r_COMPLEX :: R_EXP -> IO (Ptr R_complex)
rCOMPLEX :: REXP -> IO (Ptr R_complex)
rCOMPLEX s = withREXP s r_COMPLEX

rComplexPeek :: Ptr R_complex -> IO (CDouble, CDouble)
rComplexPeek p = do
  x <- (#peek Rcomplex, r) p
  y <- (#peek Rcomplex, i) p
  return (x,y)

rComplexPoke :: Ptr R_complex -> (CDouble, CDouble) -> IO ()
rComplexPoke p (x,y) = do
  (#poke Rcomplex, r) p x
  (#poke Rcomplex, i) p y

foreign import ccall unsafe "STRING_PTR" r_STRING_PTR :: R_EXP -> IO (Ptr R_EXP)
rSTRING_PTR :: REXP -> IO (Ptr REXP)
rSTRING_PTR s = castPtr =.< withREXP s r_STRING_PTR

foreign import ccall safe "STRING_ELT" r_STRING_ELT :: R_EXP -> CInt -> IO R_EXP
rSTRING_ELT :: REXP -> Int -> IO REXP
rSTRING_ELT s i = withREXP s $ \s -> r_STRING_ELT s (ii i) >>= newREXP

foreign import ccall safe "SET_STRING_ELT" r_SET_STRING_ELT :: R_EXP -> CInt -> R_EXP -> IO R_EXP
rSET_STRING_ELT :: REXP -> Int -> REXP -> IO ()
rSET_STRING_ELT s i v = void $ withREXP s $ \s -> withREXP v $ \v -> r_SET_STRING_ELT s (ii i) v

-- FIXME do not use:
foreign import ccall safe "VECTOR_PTR" r_VECTOR_PTR :: R_EXP -> IO (Ptr R_EXP)
rVECTOR_PTR :: REXP -> IO (Ptr REXP)
rVECTOR_PTR s = castPtr =.< withREXP s r_VECTOR_PTR

foreign import ccall safe "VECTOR_ELT" r_VECTOR_ELT :: R_EXP -> CInt -> IO R_EXP
rVECTOR_ELT :: REXP -> Int -> IO REXP
rVECTOR_ELT s i = withREXP s $ \s -> r_VECTOR_ELT s (ii i) >>= newREXP

foreign import ccall safe "SET_VECTOR_ELT" r_SET_VECTOR_ELT :: R_EXP -> CInt -> R_EXP -> IO R_EXP
rSET_VECTOR_ELT :: REXP -> Int -> REXP -> IO ()
rSET_VECTOR_ELT s i v = void $ withREXP s $ \s -> withREXP v $ \v -> r_SET_VECTOR_ELT s (ii i) v

#sexpfun TAG
#sexpfun CAR
#sexpfun CDR

foreign import ccall unsafe "Rf_cons" r_Cons :: R_EXP -> R_EXP -> IO R_EXP
rCons :: REXP -> REXP -> IO REXP
rCons a d = withREXP a $ \a -> withREXP d (r_Cons a >=> newREXP)

foreign import ccall unsafe "Rf_lcons" r_LCons :: R_EXP -> R_EXP -> IO R_EXP
rLCons :: REXP -> REXP -> IO REXP
rLCons a d = withREXP a $ \a -> withREXP d (r_LCons a >=> newREXP)

#sexpset _TAG
#sexpset CAR
#sexpset CDR

#sexpfun FRAME
#sexpfun ENCLOS

#sexpfun FORMALS
#sexpfun BODY
#sexpfun CLOENV

#sexpset _FORMALS
#sexpset _BODY
#sexpset _CLOENV

foreign import ccall safe "Rf_eval" r_Eval :: R_EXP -> R_EXP -> IO R_EXP
rEval :: REXP -> REXP -> IO REXP
rEval s e = withREXP s $ \s -> withREXP e $ r_Eval s >=> newREXP

foreign import ccall safe "R_tryEval" r_TryEval :: R_EXP -> R_EXP -> Ptr CInt -> IO R_EXP
rTryEval :: REXP -> REXP -> IO (Maybe REXP)
rTryEval s e = withREXP s $ \s -> withREXP e $ \e -> alloca $ \r -> do
  v <- r_TryEval s e r
  r <- peek r
  if r /= 0
    then return Nothing
    else Just =.< newREXP v
  
