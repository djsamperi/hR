{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleContexts, FlexibleInstances, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Foreign.R.SEXP
  ( sTYPEOF
  , unsafeCastSEXP, castSEXP, anySEXP
  , maybeSEXP, liftMaybeSEXP
  , sNothing, sJust, sMaybe
  , SDATA(..)
  , sMissingArg, sUnboundValue
  , sNaString, sBlankString
  , sGlobalEnv, sEmptyEnv, sBaseEnv
  , sBracket2Symbol, sBracketSymbol, sBraceSymbol, sClassSymbol, sDimNamesSymbol, sDimSymbol, sDollarSymbol, sDotsSymbol, sDropSymbol, sLevelsSymbol, sModeSymbol, sNamesSymbol, sRowNamesSymbol, sSeedsSymbol, sTspSymbol
  , sCharString, sAllocString
  , sATTRIB, sSET_ATTRIB
  , sGetAttrib, sSetAttrib
  , sEval, sTryEval
  ) where

import Control.Monad
import Foreign
import Foreign.C.String

import Foreign.R.Util
import Foreign.R.Types
import Foreign.R.Internals

unsafeCastSEXP :: SEXP a -> SEXP b
unsafeCastSEXP = SEXP . unSEXP

sTYPEOF :: SEXP a -> IO SEXPTYPE
sTYPEOF = rTYPEOF . unSEXP

maybeSEXP :: forall a . SEXP (Maybe a) -> IO (Maybe (SEXP a))
maybeSEXP s = do
  t <- sTYPEOF s
  return $ if t == NILSXP
    then Nothing
    else Just (unsafeCastSEXP s :: SEXP a)

liftMaybeSEXP :: (SEXP a -> IO b) -> SEXP (Maybe a) -> IO (Maybe b)
liftMaybeSEXP f = maybe (return Nothing) (Just .=< f) <=< maybeSEXP

sNothing :: IO (SEXP (Maybe a))
sNothing = unsafeCastSEXP =.< sNilValue

sJust :: SEXP a -> SEXP (Maybe a)
sJust = unsafeCastSEXP

sMaybe :: (a -> IO (SEXP b)) -> Maybe a -> IO (SEXP (Maybe b))
sMaybe _ Nothing = sNothing
sMaybe f (Just x) = sJust =.< f x

sNilValue :: IO (SEXP SNIL)
sNilValue = SEXP =.< rNilValue
sUnboundValue :: IO (SEXP SSYM)
sUnboundValue = SEXP =.< rUnboundValue
sMissingArg :: IO (SEXP SSYM)
sMissingArg = SEXP =.< rMissingArg
sGlobalEnv :: IO (SEXP SENV)
sGlobalEnv = SEXP =.< rGlobalEnv
sEmptyEnv :: IO (SEXP SENV)
sEmptyEnv = SEXP =.< rEmptyEnv
sBaseEnv :: IO (SEXP SENV)
sBaseEnv = SEXP =.< rBaseEnv
sBracket2Symbol :: IO (SEXP SSYM)
sBracket2Symbol = SEXP =.< rBracket2Symbol
sBracketSymbol :: IO (SEXP SSYM)
sBracketSymbol = SEXP =.< rBracketSymbol
sBraceSymbol :: IO (SEXP SSYM)
sBraceSymbol = SEXP =.< rBraceSymbol
sClassSymbol :: IO (SEXP SSYM)
sClassSymbol = SEXP =.< rClassSymbol
sDimNamesSymbol :: IO (SEXP SSYM)
sDimNamesSymbol = SEXP =.< rDimNamesSymbol
sDimSymbol :: IO (SEXP SSYM)
sDimSymbol = SEXP =.< rDimSymbol
sDollarSymbol :: IO (SEXP SSYM)
sDollarSymbol = SEXP =.< rDollarSymbol
sDotsSymbol :: IO (SEXP SSYM)
sDotsSymbol = SEXP =.< rDotsSymbol
sDropSymbol :: IO (SEXP SSYM)
sDropSymbol = SEXP =.< rDropSymbol
sLevelsSymbol :: IO (SEXP SSYM)
sLevelsSymbol = SEXP =.< rLevelsSymbol
sModeSymbol :: IO (SEXP SSYM)
sModeSymbol = SEXP =.< rModeSymbol
sNamesSymbol :: IO (SEXP SSYM)
sNamesSymbol = SEXP =.< rNamesSymbol
sRowNamesSymbol :: IO (SEXP SSYM)
sRowNamesSymbol = SEXP =.< rRowNamesSymbol
sSeedsSymbol :: IO (SEXP SSYM)
sSeedsSymbol = SEXP =.< rSeedsSymbol
sTspSymbol :: IO (SEXP SSYM)
sTspSymbol = SEXP =.< rTspSymbol
sNaString :: IO (SEXPVec SCHAR)
sNaString = SEXP =.< rNaString
sBlankString :: IO (SEXPVec SCHAR)
sBlankString = SEXP =.< rBlankString

anySEXP :: SEXP a -> SEXPa
anySEXP = unsafeCastSEXP

castSEXP :: forall a m . (Monad m, SType a) => SEXPa -> IO (m (SEXP a))
castSEXP s = do
  t <- sTYPEOF s
  return $ if t == sTypeOf (undefined :: a)
    then return $ unsafeCastSEXP s
    else fail ("castSEXP: " ++ show t)

class SDATA a where
  sRead :: SEXP a -> IO a
  sWrite :: SEXP a -> a -> IO ()
  sAlloc :: a -> IO (SEXP a)
  sWrite _ _ = fail "sWrite: read only data"
  sAlloc _ = fail "sAlloc: read only data"

instance SDATA SNIL where
  sRead _ = return SNIL
  sWrite _ SNIL = nop
  sAlloc SNIL = sNilValue

instance SDATA a => SDATA (Maybe a) where
  sRead = liftMaybeSEXP sRead
  sAlloc = sMaybe sAlloc

class (Storable a, SType (Vector a), SDATA (Vector a)) => SVector a where
  sVectorLength :: SEXPVec a -> IO Int
  sVectorPtr :: SEXPVec a -> IO (Ptr a)
  sVectorGet :: SEXPVec a -> Int -> IO a
  sVectorSet :: SEXPVec a -> Int -> a -> IO ()
  sVectorRead :: SEXPVec a -> IO (Vector a)
  sVectorWrite :: SEXPVec a -> Vector a -> IO ()
  sVectorAlloc :: Vector a -> IO (SEXPVec a)

  sVectorLength = rLENGTH . unSEXP
  sVectorGet s i = do
    p <- sVectorPtr s
    peekElemOff p i
  sVectorSet s i v = do
    p <- sVectorPtr s
    pokeElemOff p i v
  sVectorRead s = do
    n <- sVectorLength s
    peekArray n =<< sVectorPtr s
  sVectorWrite s l = do
    n <- sVectorLength s
    p <- sVectorPtr s
    pokeArray p (take n l)
  sVectorAlloc l = do
    let n = length l
    s <- SEXP =.< rAllocVector (sTypeOf l) n
    p <- sVectorPtr s
    pokeArray p l
    return s

instance SDATA SSYM where 
  sRead = SSYM . SEXP .=< rPRINTNAME . unSEXP
  sAlloc = SEXP .=< rInstallChar . unSEXP . ssymPRINTNAME

instance SDATA SLIST where
  sRead (SEXP s) = do
    a <- rCAR s
    d <- rCDR s
    t <- rTAG s
    return $ SLIST (SEXP a) (SEXP d) (SEXP t)
  sWrite (SEXP s) (SLIST a d t) = do
    rSETCAR s (unSEXP a)
    rSETCDR s (unSEXP d)
    rSET_TAG s (unSEXP t)
  sAlloc (SLIST a d t) = do
    c <- rCons (unSEXP a) (unSEXP d)
    rSET_TAG c (unSEXP t)
    return $ SEXP c

instance SDATA SCLO where
  sRead (SEXP s) = do
    a <- rFORMALS s
    b <- rBODY s
    e <- rCLOENV s
    return $ SCLO (SEXP a) (SEXP b) (SEXP e)
  sWrite (SEXP s) (SCLO a b e) = do
    rSET_FORMALS s (unSEXP a)
    rSET_BODY s (unSEXP b)
    rSET_CLOENV s (unSEXP e)
  sAlloc x = do
    s <- SEXP =.< rAllocREXP (sTypeOf x)
    sWrite s x
    return s

instance SDATA SENV where
  sRead (SEXP s) = do
    f <- rFRAME s
    e <- rENCLOS s
    return $ SENV (SEXP f) (SEXP e)

instance SDATA SLANG where
  sRead (SEXP s) = do
    a <- rCAR s
    d <- rCDR s
    return $ SLANG (SEXP a) (SEXP d)
  sWrite (SEXP s) (SLANG a d) = do
    rSETCAR s (unSEXP a)
    rSETCDR s (unSEXP d)
  sAlloc (SLANG a d) =
    SEXP =.< rLCons (unSEXP a) (unSEXP d)

instance SDATA SCHAR where
  sRead s = sVectorGet (unsafeCastSEXP s :: SEXPVec SCHAR) 0
  sWrite s = sVectorSet (unsafeCastSEXP s :: SEXPVec SCHAR) 0
  sAlloc x = unsafeCastSEXP =.< sVectorAlloc [x]
instance SDATA (Vector SCHAR) where 
  sRead = sVectorRead
  sWrite = sVectorWrite
  sAlloc = sVectorAlloc
instance SVector SCHAR where 
  sVectorPtr = rCHAR . unSEXP

sCharString :: SEXPVec SCHAR -> IO String
sCharString s = do
  n <- sVectorLength s
  p <- sVectorPtr s
  peekCStringLen (p, n)

sAllocString :: String -> IO (SEXPVec SCHAR)
sAllocString = SEXP .=< rMkChar

deriving instance Storable SLGL
instance SDATA SLGL where
  sRead s = sVectorGet (unsafeCastSEXP s :: SEXPVec SLGL) 0
  sWrite s = sVectorSet (unsafeCastSEXP s :: SEXPVec SLGL) 0
  sAlloc x = unsafeCastSEXP =.< sVectorAlloc [x]
instance SDATA (Vector SLGL) where
  sRead = sVectorRead
  sWrite = sVectorWrite
  sAlloc = sVectorAlloc
instance SVector SLGL where
  sVectorPtr = castPtr .=< rLOGICAL . unSEXP

instance SDATA SINT where
  sRead s = sVectorGet (unsafeCastSEXP s :: SEXPVec SINT) 0
  sWrite s = sVectorSet (unsafeCastSEXP s :: SEXPVec SINT) 0
  sAlloc x = unsafeCastSEXP =.< sVectorAlloc [x]
instance SDATA (Vector SINT) where
  sRead = sVectorRead
  sWrite = sVectorWrite
  sAlloc = sVectorAlloc
instance SVector SINT where
  sVectorPtr = castPtr .=< rINTEGER . unSEXP

instance SDATA SREAL where
  sRead s = sVectorGet (unsafeCastSEXP s :: SEXPVec SREAL) 0
  sWrite s = sVectorSet (unsafeCastSEXP s :: SEXPVec SREAL) 0
  sAlloc x = unsafeCastSEXP =.< sVectorAlloc [x]
instance SDATA (Vector SREAL) where
  sRead = sVectorRead
  sWrite = sVectorWrite
  sAlloc = sVectorAlloc
instance SVector SREAL where
  sVectorPtr = castPtr .=< rREAL . unSEXP


instance SDATA SCPLX where
  sRead s = sVectorGet (unsafeCastSEXP s :: SEXPVec SCPLX) 0
  sWrite s = sVectorSet (unsafeCastSEXP s :: SEXPVec SCPLX) 0
  sAlloc x = unsafeCastSEXP =.< sVectorAlloc [x]
instance SDATA (Vector SCPLX) where
  sRead = sVectorRead
  sWrite = sVectorWrite
  sAlloc = sVectorAlloc
instance SVector SCPLX where
  sVectorPtr = castPtr .=< rCOMPLEX . unSEXP

instance SDATA SRAW where
  sRead s = sVectorGet (unsafeCastSEXP s :: SEXPVec SRAW) 0
  sWrite s = sVectorSet (unsafeCastSEXP s :: SEXPVec SRAW) 0
  sAlloc x = unsafeCastSEXP =.< sVectorAlloc [x]
instance SDATA (Vector SRAW) where
  sRead = sVectorRead
  sWrite = sVectorWrite
  sAlloc = sVectorAlloc
instance SVector SRAW where
  sVectorPtr = castPtr .=< rRAW . unSEXP

instance SDATA SANY where
  sRead _ = fail "sGet: abstract data"

instance Storable (SEXP a) where
  sizeOf = sizeOf . unSEXP
  alignment = alignment . unSEXP
  peek = SEXP .=< peek . castPtr
  poke p v = poke (castPtr p) (unSEXP v)

instance SDATA (Vector SSTR) where
  sRead = sVectorRead
  sWrite = sVectorWrite
  sAlloc = sVectorAlloc
instance SVector SSTR where 
  sVectorPtr = castPtr .=< rSTRING_PTR . unSEXP
  --sVectorGet s i = SEXP =.< rSTRING_ELT (unSEXP s) i
  sVectorSet s i v = rSET_STRING_ELT (unSEXP s) i (unSEXP v)
  --sVectorRead s = mapM (sVectorGet s) . enumFromTo 0 . pred =<< sVectorLength s
  sVectorWrite s l = do
    n <- sVectorLength s
    zipWithM_ (sVectorSet s) (enumFromTo 0 (pred n)) l

instance SDATA (Vector SVEC) where
  sRead = sVectorRead
  sWrite = sVectorWrite
  sAlloc = sVectorAlloc
instance SVector SVEC where
  sVectorPtr = castPtr .=< rVECTOR_PTR . unSEXP
  sVectorGet s i = SEXP =.< rVECTOR_ELT (unSEXP s) i
  sVectorSet s i v = rSET_VECTOR_ELT (unSEXP s) i (unSEXP v)
  sVectorRead s = mapM (sVectorGet s) . enumFromTo 0 . pred =<< sVectorLength s
  sVectorWrite s l = do
    n <- sVectorLength s
    zipWithM_ (sVectorSet s) (enumFromTo 0 (pred n)) l
  sVectorAlloc l = do
    let n = length l
    s <- SEXP =.< rAllocVector VECSXP n
    zipWithM_ (sVectorSet s) (enumFromTo 0 (pred n)) l
    return s

deriving instance Storable SEXPR
instance SDATA (Vector SEXPR) where
  sRead = sVectorRead
  sWrite = sVectorWrite
  sAlloc = sVectorAlloc
instance SVector SEXPR where
  sVectorPtr = castPtr .=< rVECTOR_PTR . unSEXP
  sVectorGet s i = SEXPR . SEXP =.< rVECTOR_ELT (unSEXP s) i
  sVectorSet s i v = rSET_VECTOR_ELT (unSEXP s) i (unSEXP (unSEXPR v))
  sVectorRead s = mapM (sVectorGet s) . enumFromTo 0 . pred =<< sVectorLength s
  sVectorWrite s l = do
    n <- sVectorLength s
    zipWithM_ (sVectorSet s) (enumFromTo 0 (pred n)) l
  sVectorAlloc l = do
    let n = length l
    s <- SEXP =.< rAllocVector EXPRSXP n
    zipWithM_ (sVectorSet s) (enumFromTo 0 (pred n)) l
    return s

sATTRIB :: SEXP a -> IO (SEXP (Maybe SLIST))
sATTRIB = SEXP .=< rATTRIB . unSEXP

sSET_ATTRIB :: SEXP a -> SEXP (Maybe SLIST) -> IO ()
sSET_ATTRIB s = rSET_ATTRIB (unSEXP s) . unSEXP

sGetAttrib :: SEXP a -> String -> IO SEXPa
sGetAttrib s a = SEXP =.< rGetAttrib (unSEXP s) =<< rInstall a

sSetAttrib :: SEXP a -> String -> SEXP b -> IO ()
sSetAttrib s a v = do
  a <- rInstall a
  rSetAttrib (unSEXP s) a (unSEXP v)

sEval :: SEXPa -> SEXP SENV -> IO SEXPa
sEval s e = SEXP =.< rEval (unSEXP s) (unSEXP e)

sTryEval :: SEXPa -> SEXP SENV -> IO (Maybe SEXPa)
sTryEval s e = fmap SEXP =.< rTryEval (unSEXP s) (unSEXP e)
