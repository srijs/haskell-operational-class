{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, GADTs #-}

module Control.Monad.Operational.Class where

import Data.Monoid (Monoid)

import Control.Monad (liftM, join)
import Control.Monad.Operational
import Control.Monad.Trans.Class (MonadTrans, lift)

import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT)
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT)
import Control.Monad.Trans.Cont (ContT)
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT)
import qualified Control.Monad.Trans.Writer.Lazy as Lazy (WriterT)
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST)
import qualified Control.Monad.Trans.RWS.Lazy as Lazy (RWST)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.List (ListT)

class Monad m => MonadProgram instr m | m -> instr where
  wrap :: Program instr (m a) -> m a

wrapT :: (m ~ t n, Monad m, MonadTrans t, MonadProgram instr n) => Program instr (m a) -> m a
wrapT = join . lift . wrap . fmap return

liftP :: MonadProgram instr m => Program instr a -> m a
liftP p = wrap $ liftM return p

instance Monad m => MonadProgram instr (ProgramT instr m) where
  wrap = eval . view
    where eval (Return a) = a
          eval (i :>>= k) = singleton i >>= wrap . k

instance (MonadProgram instr m) => MonadProgram instr (ReaderT e m) where
  wrap = wrapT

instance (MonadProgram instr m) => MonadProgram instr (Strict.StateT s m) where
  wrap = wrapT

instance (MonadProgram instr m) => MonadProgram instr (Lazy.StateT s m) where
  wrap = wrapT

instance (MonadProgram instr m) => MonadProgram instr (ContT r m) where
  wrap = wrapT

instance (MonadProgram instr m, Monoid w) => MonadProgram instr (Strict.WriterT w m) where
  wrap = wrapT

instance (MonadProgram instr m, Monoid w) => MonadProgram instr (Lazy.WriterT w m) where
  wrap = wrapT

instance (MonadProgram instr m, Monoid w) => MonadProgram instr (Strict.RWST r w s m) where
  wrap = wrapT

instance (MonadProgram instr m, Monoid w) => MonadProgram instr (Lazy.RWST r w s m) where
  wrap = wrapT

instance (MonadProgram instr m) => MonadProgram instr (MaybeT m) where
  wrap = wrapT

instance (MonadProgram instr m) => MonadProgram instr (IdentityT m) where
  wrap = wrapT

instance (MonadProgram instr m) => MonadProgram instr (ListT m) where
  wrap = wrapT