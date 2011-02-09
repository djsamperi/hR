{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, ScopedTypeVariables, FlexibleContexts #-}
module Foreign.R.SExp
  ( SExp(..)
  , SList
  , SAssocs
  , SFormals
  , SEnv(..)
  , sEnvFrame, sEnvEnclos
  , SClo(..)
  , SLang(..)
  , sLogical, fromSLogical
  , sImp
  , sCall
  ) where

import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import Foreign.C.String
import Unsafe.Coerce

import Foreign.R.Util
import Foreign.R.Types
import Foreign.R.Internals
import Foreign.R.SEXP

type SList = [(Maybe SName, SExp)]

type SAssocs = Map.Map SName SExp

data SEnv 
  = SEnv (SEXP SENV)
  | SEnvGlobal
  | SEnvEmpty
  | SEnvBase
  deriving (Eq, Show)

type SFormals = [(SName, Maybe SExp)]

data SClo = SClo
  { sCloFormals :: SFormals
  , sCloBody :: SExp
  , sCloEnv :: SEnv
  } deriving (Eq, Show)

data SLang = SLang
  { sLangFunction :: SExp -- ^ The function to call, usually a symbol (CAR)
  , sLangArgs :: SList -- ^ Arguments to pass (CDR)
  } deriving (Eq, Show)

data SExp
  = SNull -- ^ The NULL value, empty lists, etc.
  | SSymbol !SName -- ^ An identifier
  | SUnboundValue
  | SMissingArg
  | SList !SList -- ^ A tagged pairlist
  | SClosure !SClo -- ^ A function definition
  | SEnvironment !SEnv -- ^ A scope containing bindings [mutable]
  -- promise -- ^ A thunk (value is R_UnboundValue for uninitialized)
  | SLanguage !SLang -- ^ An unevaluated function call
  | SSpecial (SEXP SSPECIAL) -- ^ A special language construct (funtable offset only, unevaluated args) [internal]
  | SBuiltin (SEXP SBUILTIN) -- ^ A language builtin function (funtable offset only, evaluated args) [internal]
  | SChar !SString -- ^ A scalar string (vector of characters) [internal]
  | SLogical (Vector SLogical)
  | SInteger (Vector SInteger)
  | SReal (Vector SReal)
  | SComplex (Vector SComplex)
  | SString (Vector SString)
  -- dotdotdot
  -- any -- ^ Pseudo-type only for signatures
  | SVector (Vector SExp) -- ^ A list
  | SExpression (Vector SExp) -- ^ A list of statements (language calls, symbols, etc.), as parsed from a file
  -- bytecode
  -- extptr
  -- weakref
  | SRaw (Vector SRaw)
  -- s4

  | SAttributes SAssocs SExp -- ^ Transparent attributes
  | SFactor (Vector SString) (Vector SInteger)
  deriving (Show, Eq)

instance NA SExp where
  na = SLogical [na]
  isNA (SChar Nothing) = True
  isNA (SLogical [x]) = isNA x
  isNA (SInteger [x]) = isNA x
  isNA (SReal [x]) = isNA x
  isNA (SComplex [x]) = isNA x
  isNA (SString [x]) = isNA x
  isNA _ = False


class SDATA a => SData a b where
  sImp :: a -> IO b
  sExp :: b -> IO a
  sGet :: SEXP a -> IO b
  sPut :: SEXP a -> b -> IO ()
  sNew :: b -> IO (SEXP a)
  sImp _ = fail "sImp: no conversion"
  sExp _ = fail "sExp: no conversion"
  sGet = sImp <=< sRead
  sPut s = sWrite s <=< sExp
  sNew = sAlloc <=< sExp


instance SData SNIL () where 
  sImp SNIL = return ()
  sExp () = return SNIL

instance SData SSYM SName where 
  sImp = sCharString . ssymPRINTNAME
  sExp = SSYM .=< sNew . Just
  sNew = SEXP .=< rInstall

instance SData a [b] => SData (Maybe a) [b] where
  sImp Nothing = return []
  sImp (Just l) = sImp l
  sExp [] = return $ Nothing
  sExp l = Just =.< sExp l
  sNew [] = sNothing
  sNew l = sJust =.< sNew l

instance SData SLIST SList where
  sImp (SLIST a d t) = do
    a <- sGet a
    t <- liftMaybeSEXP sGet t
    ((t,a) :) =.< sGet d
  sExp ((t,a):l) = do
    a <- sNew a
    d <- sNew l
    t <- sMaybe sNew t
    return $ SLIST a d t
  sExp [] = fail "sExp: empty SLIST"
  sNew l@((t',_):_) = do
    SLIST a d t <- sExp l
    c <- rCons (unSEXP a) (unSEXP d)
    whenJust t' $ const $ rSET_TAG c (unSEXP t)
    return $ SEXP c
  sNew [] = fail "sNew: empty SLIST"

instance SData SLIST SFormals where
  sImp (SLIST a d t) = do
    a <- liftMaybeSEXP sGet (unsafeCastSEXP a :: SEXP (Maybe SANY))
    t <- sGet (unsafeCastSEXP t :: SEXP SSYM)
    ((t,a) :) =.< sGet d
  sExp ((t,a):l) = do
    a <- sMaybe sNew a :: IO (SEXP (Maybe SANY))
    d <- sNew l
    t <- sNew t
    return $ SLIST (unsafeCastSEXP a :: SEXPa) d t
  sNew l = do
    SLIST a d t <- sExp l
    c <- rCons (unSEXP a) (unSEXP d)
    rSET_TAG c (unSEXP t)
    return $ SEXP c

sAssocsFromList :: SList -> SAssocs
sAssocsFromList = Map.fromListWith const . mapMaybe (\(t,a) -> fmap (\t -> (t,a)) t)

instance SData (Maybe SLIST) SAssocs where
  sImp = sAssocsFromList .=< sImp
  sNew = sNew . map (\(t,v) -> (Just t,v)) . Map.assocs

instance SData SCLO SClo where
  sImp (SCLO a b e) = do
    a <- sGet a
    b <- sGet b
    e <- sGet e
    return $ SClo a b e
  sExp (SClo a b e) = do
    a <- sNew a
    b <- sNew b
    e <- sNew e
    return $ SCLO a b e

instance SData SENV SEnv where
  sGet s = do
    g <- sGlobalEnv
    e <- sEmptyEnv
    b <- sBaseEnv
    return $ if s == g then SEnvGlobal
      else if s == e then SEnvEmpty
      else if s == b then SEnvBase
      else SEnv s
  sNew SEnvGlobal = sGlobalEnv
  sNew SEnvEmpty = sEmptyEnv
  sNew SEnvBase = sBaseEnv
  sNew (SEnv s) = return s
instance SData (Maybe SENV) SEnv where
  sGet = maybe (return SEnvEmpty) sGet <=< maybeSEXP
  sNew = sJust .=< sNew

sEnvFrame :: SData (Maybe SLIST) b => SEnv -> IO b
sEnvFrame = sGet . senvFRAME <=< sRead <=< sNew

sEnvEnclos :: SData (Maybe SENV) b => SEnv -> IO b
sEnvEnclos = sGet . senvENCLOS <=< sRead <=< sNew

instance SData SLANG SLang where
  sImp (SLANG a d) = do
    a <- sGet a
    SLang a =.< sGet d
  sExp (SLang a l) = do
    a <- sNew a
    SLANG a =.< sNew l

instance SData SCHAR Char where
  sImp = return . castCCharToChar
  sExp = return . castCharToCChar
instance SData (Vector SCHAR) SString where
  sImp = return . Just . map castCCharToChar -- XXX can't detect NA
  sExp (Just s) = return $ map castCharToCChar s
  sExp Nothing = sExp (Just "NA") -- XXX
  sGet s = do
    n <- sNaString
    if s == n
      then return Nothing
      else Just =.< sCharString s
  sNew Nothing = sNaString
  sNew (Just "") = sBlankString
  sNew (Just x) = sAllocString x

instance SData SLGL SLogical where
  sImp (SLGL 0) = return FALSE
  sImp (SLGL i)
    | isNA i = return NA
    | otherwise = return TRUE
  sExp FALSE = return $ SLGL 0
  sExp TRUE = return $ SLGL 1
  sExp NA = return $ SLGL na
instance SData (Vector SLGL) (Vector SLogical) where
  sImp = mapM sImp
  sExp = mapM sExp

sLogical :: Bool -> SLogical
sLogical False = FALSE
sLogical True = TRUE

fromSLogical :: SLogical -> Bool
fromSLogical FALSE = False
fromSLogical TRUE = True
fromSLogical NA = error "Foreign.R.Types.fromSLogical: NA"

instance SData SINT SInteger where
  sImp = return . ii
  sExp = return . ii
instance SData (Vector SINT) (Vector SInteger) where
  sImp = return . map ii
  sExp = return . map ii

instance SData SREAL SReal where
  sImp = return . unsafeCoerce
  sExp = return . unsafeCoerce
instance SData (Vector SREAL) (Vector SReal) where
  sImp = return . map unsafeCoerce
  sExp = return . map unsafeCoerce

instance SData SCPLX SComplex where
  sImp (x,y) = return (unsafeCoerce x:+unsafeCoerce y)
  sExp (x :+ y) = return (unsafeCoerce x,unsafeCoerce y)
instance SData (Vector SCPLX) (Vector SComplex) where
  sImp = mapM sImp
  sExp = mapM sExp

instance SData (Vector SSTR) (Vector SString) where
  sImp = mapM sGet
  sExp = mapM sNew

instance SData SANY SExp where
  sGet s' = do
    t <- sTYPEOF s'
    a <- liftMaybeSEXP (sGet . sJust) =<< sATTRIB s'
    let af = case (t, a) of 
	  (_, Nothing) -> id
	  (INTSXP, Just [(Just "levels", SString l), (Just "class", SString [Just "factor"])]) -> liftM (\(SInteger x) -> SFactor l x)
	  (INTSXP, Just [(Just "class", SString [Just "factor"]), (Just "levels", SString l)]) -> liftM (\(SInteger x) -> SFactor l x)
	  (_, Just a) -> liftM $ SAttributes $ sAssocsFromList a
    af $ case t of
      NILSXP -> return SNull
      SYMSXP -> let s = unsafeCastSEXP s' :: SEXP SSYM in do
	m <- sMissingArg
	u <- sUnboundValue
	if s == u
	  then return SUnboundValue
	  else if s == m
	    then return SMissingArg
	    else SSymbol =.< sGet s
      LISTSXP -> SList =.< sGet (unsafeCastSEXP s' :: SEXP SLIST)
      ENVSXP -> SEnvironment =.< sGet (unsafeCastSEXP s' :: SEXP SENV)
      CLOSXP -> SClosure =.< sGet (unsafeCastSEXP s' :: SEXP SCLO)
      LANGSXP -> SLanguage =.< sGet (unsafeCastSEXP s' :: SEXP SLANG)
      SPECIALSXP -> return $ SSpecial (unsafeCastSEXP s' :: SEXP SSPECIAL)
      BUILTINSXP -> return $ SBuiltin (unsafeCastSEXP s' :: SEXP SBUILTIN)
      CHARSXP -> SChar =.< sGet (unsafeCastSEXP s' :: SEXPVec SCHAR)
      LGLSXP -> SLogical =.< sGet (unsafeCastSEXP s' :: SEXPVec SLGL)
      INTSXP -> SInteger =.< sGet (unsafeCastSEXP s' :: SEXPVec SINT)
      REALSXP -> SReal =.< sGet (unsafeCastSEXP s' :: SEXPVec SREAL)
      CPLXSXP -> SComplex =.< sGet (unsafeCastSEXP s' :: SEXPVec SCPLX)
      STRSXP -> SString =.< sGet (unsafeCastSEXP s' :: SEXPVec SSTR)
      VECSXP -> SVector =.< sGet (unsafeCastSEXP s' :: SEXPVec SVEC)
      EXPRSXP -> SExpression =.< sGet (unsafeCastSEXP s' :: SEXPVec SEXPR)
      RAWSXP -> SRaw =.< sGet (unsafeCastSEXP s' :: SEXPVec SRAW)
      _ -> fail ("sGet: unsupported type " ++ show t)

  sNew SNull = anySEXP =.< (sNew () :: IO (SEXP SNIL))
  sNew (SSymbol x) = anySEXP =.< (sNew x :: IO (SEXP SSYM))
  sNew SUnboundValue = anySEXP =.< sUnboundValue
  sNew SMissingArg = anySEXP =.< sMissingArg
  sNew (SList x) = anySEXP =.< (sNew x :: IO (SEXP SLIST))
  sNew (SEnvironment x) = anySEXP =.< (sNew x :: IO (SEXP SENV))
  sNew (SClosure x) = anySEXP =.< (sNew x :: IO (SEXP SCLO))
  sNew (SLanguage x) = anySEXP =.< (sNew x :: IO (SEXP SLANG))
  sNew (SSpecial x) = return $ anySEXP x
  sNew (SBuiltin x) = return $ anySEXP x
  sNew (SChar x) = anySEXP =.< (sNew x :: IO (SEXPVec SCHAR))
  sNew (SLogical x) = anySEXP =.< (sNew x :: IO (SEXPVec SLGL))
  sNew (SInteger x) = anySEXP =.< (sNew x :: IO (SEXPVec SINT))
  sNew (SReal x) = anySEXP =.< (sNew x :: IO (SEXPVec SREAL))
  sNew (SComplex x) = anySEXP =.< (sNew x :: IO (SEXPVec SCPLX))
  sNew (SString x) = anySEXP =.< (sNew x :: IO (SEXPVec SSTR))
  sNew (SVector x) = anySEXP =.< (sNew x :: IO (SEXPVec SVEC))
  sNew (SExpression x) = anySEXP =.< (sNew x :: IO (SEXPVec SEXPR))
  sNew (SRaw x) = anySEXP =.< (sNew x :: IO (SEXPVec SRAW))
  sNew (SAttributes a x) = do
    s <- sNew x
    sSetAttributes s a
    return s
  sNew (SFactor l x) = sNew $ SAttributes (Map.fromList [("class", SString [Just "factor"]), ("levels", SString l)]) (SInteger x)

instance SData (Vector SVEC) (Vector SExp) where
  sImp = mapM sGet
  sExp = mapM sNew

instance SData (Vector SEXPR) (Vector SExp) where
  sImp = mapM (sGet . unSEXPR)
  sExp = mapM (SEXPR .=< sNew)

instance SData SRAW SRaw where
  sImp = return
  sExp = return
instance SData (Vector SRAW) (Vector SRaw) where
  sImp = return
  sExp = return

sGetAttributes :: SData (Maybe SLIST) b => SEXP a -> IO b
sGetAttributes = sGet <=< sATTRIB

sSetAttributes :: SData (Maybe SLIST) b => SEXP a -> b -> IO ()
sSetAttributes s = sSET_ATTRIB s <=< sNew

sCall :: SName -> SList -> IO SExp
sCall f a = do
  c :: SEXP SLANG <- sNew (SLang (SSymbol f) a)
  sGet =<< sEval (anySEXP c) =<< sGlobalEnv
