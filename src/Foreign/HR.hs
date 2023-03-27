{-# LANGUAGE NoImplicitPrelude #-}

{-|

  General naming conventions used herein:

    [@[rR]_.*@] represent bare R calls and data
    
    [@[rR][A-Z].*@] are memory-managed but un-typed R calls and data

    [@[sS](_.*|[A-Z_]*)@] represent typed and wrapped imperative R calls and data (in IO)

    [@[sS].*@] and all others are functional equivalent representation and conversions

-}
module Foreign.HR
  (
  ) where
