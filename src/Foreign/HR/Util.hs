module Foreign.HR.Util 
  ( ii
  , (>.)
  , (>.=)
  , (>=.)
  , (>=>)
  , (=.<)
  , (.=<)
  , (<=<)
  , nop
  , whenJust
  ) where

import Control.Monad

ii :: (Integral a, Integral b) => a -> b
ii = fromIntegral

infixl 1 >., >.=, >=.
infixr 1 =.<, .=<
(>.) :: Monad m => m a -> b -> m b
(>.=) :: Monad m => m a -> (a -> b) -> m b
(=.<) :: Monad m => (a -> b) -> m a -> m b
(>=.) :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
(.=<) :: Monad m => (b -> c) -> (a -> m b) -> a -> m c

(>.) e r = e >> return r
(>.=) e r = e >>= return . r
(=.<) r e = return . r =<< e
(>=.) e r = e >=> return . r
(.=<) r e = return . r <=< e

nop :: Monad m => m ()
nop = return ()

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust m f = maybe nop f m
