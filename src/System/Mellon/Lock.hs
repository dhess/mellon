{-# LANGUAGE MultiParamTypeClasses #-}

module System.Mellon.Lock
         ( Lock(..)
         ) where

-- | 'Lock' abstracts the machinery which manipulates the actual lock.
-- The protocol is the simplest one possible: 'lock' and 'unlock'.
--
-- Presumably only an effectful lock is useful, therefore 'Lock' is
-- parameterized on 'Monad'.
class Monad m => Lock m l where
  lock :: l -> m ()
  unlock :: l -> m ()
