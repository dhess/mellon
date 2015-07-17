module System.Mellon.Lock.Class
         ( MonadLock(..)
         ) where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.List
import Control.Monad.Reader
import qualified Control.Monad.RWS.Lazy as RWSL
import qualified Control.Monad.RWS.Strict as RWSS
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.State.Strict as SS
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer.Lazy as WL
import qualified Control.Monad.Writer.Strict as WS

class (Monad m) => MonadLock m where
  lock :: m ()
  unlock :: m ()

instance (MonadLock m) => MonadLock (IdentityT m) where
  lock = lift lock
  unlock = lift unlock

instance (MonadLock m) => MonadLock (SL.StateT s m) where
  lock = lift lock
  unlock = lift unlock

instance (MonadLock m) => MonadLock (SS.StateT s m) where
  lock = lift lock
  unlock = lift unlock

instance (MonadLock m, Monoid w) => MonadLock (WL.WriterT w m) where
  lock = lift lock
  unlock = lift unlock

instance (MonadLock m, Monoid w) => MonadLock (WS.WriterT w m) where
  lock = lift lock
  unlock = lift unlock

instance (MonadLock m) => MonadLock (ReaderT r m) where
  lock = lift lock
  unlock = lift unlock

instance (MonadLock m, Monoid w) => MonadLock (RWSL.RWST r w s m) where
  lock = lift lock
  unlock = lift unlock

instance (MonadLock m, Monoid w) => MonadLock (RWSS.RWST r w s m) where
  lock = lift lock
  unlock = lift unlock

instance (MonadLock m) => MonadLock (ExceptT e m) where
  lock = lift lock
  unlock = lift unlock

instance (MonadLock m) => MonadLock (MaybeT m) where
  lock = lift lock
  unlock = lift unlock

instance (MonadLock m) => MonadLock (ContT r m) where
  lock = lift lock
  unlock = lift unlock

instance (MonadLock m) => MonadLock (ListT m) where
  lock = lift lock
  unlock = lift unlock
