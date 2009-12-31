{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Foreign.R.Types
  ( SEXPTYPE(..)
  , R_EXP, REXP
  , SEXP(..), SEXPa

  , NA(..)
  , Vector, SEXPVec
  , SName
  , SLogical(..)
  , SInteger
  , SReal
  , SComplex, Complex((:+))
  , SString
  , SRaw

  , SType, sTypeOf
  , SNIL(..)
  , SSYM(..)
  , SLIST(..)
  , SENV(..)
  , SLANG(..)
  , SCLO(..)
  , SCHAR
  , SLGL(..)
  , SINT
  , SREAL
  , SCPLX
  , SSTR
  , SVEC
  , SEXPR(..)
  , SRAW
  , SANY

  ) where

import Data.Int
import Data.Word
import Data.Maybe
import Data.Complex
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types

data SEXPTYPE
  = NILSXP
  | SYMSXP
  | LISTSXP
  | CLOSXP
  | ENVSXP
  | PROMSXP
  | LANGSXP
  | SPECIALSXP
  | BUILTINSXP
  | CHARSXP
  | LGLSXP
  | INTSXP
  | REALSXP
  | CPLXSXP
  | STRSXP
  | DOTSXP
  | ANYSXP
  | VECSXP
  | EXPRSXP
  | BCODESXP
  | EXTPTRSXP
  | WEAKREFSXP
  | RAWSXP
  | S4SXP
  | FUNSXP
  deriving (Eq, Show)

data REXPREC
type R_EXP = Ptr REXPREC
type REXP = ForeignPtr REXPREC
newtype SEXP a = SEXP { unSEXP :: REXP } deriving (Eq, Show)
type SEXPa = SEXP SANY

class SType a where
  sTypeOf :: a -> SEXPTYPE

class Eq a => NA a where
  na :: a
  isNA :: a -> Bool
  isNA = (na ==)

instance Eq a => NA (Maybe a) where
  na = Nothing
  isNA = isNothing

type SName = String

type Vector a = [a]
type SEXPVec a = SEXP (Vector a)

data SLogical = FALSE | TRUE | NA deriving (Show, Eq)
instance Enum SLogical where
  toEnum 0 = FALSE
  toEnum x 
    | x > 0 = TRUE
    | otherwise = NA
  fromEnum FALSE = 0
  fromEnum TRUE = 1
  fromEnum NA = (-1)
instance NA SLogical where
  na = NA

type SInteger = Int32
instance NA SInteger where
  na = minBound

type SReal = Double
type SComplex = Complex SReal
type SString = Maybe String
type SRaw = Word8


data SNIL = SNIL
instance SType SNIL where sTypeOf _ = NILSXP

data SSYM = SSYM { ssymPRINTNAME :: SEXPVec SCHAR }
instance SType SSYM where sTypeOf _ = SYMSXP

data SLIST = SLIST 
  { slistCAR :: SEXPa
  , slistCDR :: SEXP (Maybe SLIST)
  , slistTAG :: SEXP (Maybe SSYM)
  }
instance SType SLIST where sTypeOf _ = LISTSXP

data SCLO = SCLO
  { scloFORMALS :: SEXP (Maybe SLIST)
  , scloBODY :: SEXPa
  , scloENV :: SEXP SENV
  }
instance SType SCLO where sTypeOf _ = CLOSXP

data SENV = SENV
  { senvFRAME :: SEXP (Maybe SLIST)
  , senvENCLOS :: SEXP (Maybe SENV)
  }
instance SType SENV where sTypeOf _ = ENVSXP

data SLANG = SLANG
  { slangCAR :: SEXPa
  , slangCDR :: SEXP (Maybe SLIST)
  }
instance SType SLANG where sTypeOf _ = LANGSXP

type SCHAR = CChar
instance SType (Vector SCHAR) where sTypeOf _ = CHARSXP

newtype SLGL = SLGL { unSLGL :: CInt } deriving (Eq)
instance NA SLGL where na = SLGL na
instance SType (Vector SLGL) where sTypeOf _ = LGLSXP

type SINT = CInt
instance NA SINT where na = minBound -- hardcoded R_NaInt
instance SType (Vector SINT) where sTypeOf _ = INTSXP

type SREAL = CDouble
instance SType (Vector SREAL) where sTypeOf _ = REALSXP

type SCPLX = (CDouble, CDouble)
instance SType (Vector SCPLX) where sTypeOf _ = CPLXSXP

type SSTR = SEXPVec SCHAR
instance SType (Vector SSTR) where sTypeOf _ = STRSXP

type SVEC = SEXPa
instance SType (Vector SVEC) where sTypeOf _ = VECSXP

newtype SEXPR = SEXPR { unSEXPR :: SEXPa }
instance SType (Vector SEXPR) where sTypeOf _ = EXPRSXP

type SRAW = SRaw
instance SType (Vector SRAW) where sTypeOf _ = RAWSXP

data SANY







foreign import ccall unsafe "r_ValueOfNA" rValueOfNA :: Double
instance NA SReal where
  na = rValueOfNA
  isNA x = decodeFloat rValueOfNA == decodeFloat x


instance NA SComplex where
  na = rValueOfNA :+ rValueOfNA
  isNA (x :+ y) = isNA x || isNA y


foreign import ccall unsafe r_ValueOfNA :: CDouble
instance NA CDouble where
  na = r_ValueOfNA -- hardcoded R_NAReal
