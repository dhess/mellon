{-# LANGUAGE MultiParamTypeClasses #-}

module System.Mellon.Lock
         ( Lock(..)
         ) where

class Monad m => Lock m l where
  lock :: l -> m ()
  unlock :: l -> m ()
