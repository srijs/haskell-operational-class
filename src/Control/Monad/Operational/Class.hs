{-# LANGUAGE Safe, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, GADTs #-}

module Control.Monad.Operational.Class where

import Data.Monoid (Monoid)

import qualified Control.Monad.Operational as Operational
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
  liftProgram :: Operational.Program instr a -> m a

liftProgramTrans :: (Monad (t n), MonadTrans t, MonadProgram instr n) => Operational.Program instr a -> t n a
liftProgramTrans = lift . liftProgram

singleton :: MonadProgram instr m => instr a -> m a
singleton = liftProgram . Operational.singleton

instance Monad m => MonadProgram instr (Operational.ProgramT instr m) where
  liftProgram = Operational.liftProgram

instance (MonadProgram instr m) => MonadProgram instr (ReaderT e m) where
  liftProgram = liftProgramTrans

instance (MonadProgram instr m) => MonadProgram instr (Strict.StateT s m) where
  liftProgram = liftProgramTrans

instance (MonadProgram instr m) => MonadProgram instr (Lazy.StateT s m) where
  liftProgram = liftProgramTrans

instance (MonadProgram instr m) => MonadProgram instr (ContT r m) where
  liftProgram = liftProgramTrans

instance (MonadProgram instr m, Monoid w) => MonadProgram instr (Strict.WriterT w m) where
  liftProgram = liftProgramTrans

instance (MonadProgram instr m, Monoid w) => MonadProgram instr (Lazy.WriterT w m) where
  liftProgram = liftProgramTrans

instance (MonadProgram instr m, Monoid w) => MonadProgram instr (Strict.RWST r w s m) where
  liftProgram = liftProgramTrans

instance (MonadProgram instr m, Monoid w) => MonadProgram instr (Lazy.RWST r w s m) where
  liftProgram = liftProgramTrans

instance (MonadProgram instr m) => MonadProgram instr (MaybeT m) where
  liftProgram = liftProgramTrans

instance (MonadProgram instr m) => MonadProgram instr (IdentityT m) where
  liftProgram = liftProgramTrans

instance (MonadProgram instr m) => MonadProgram instr (ListT m) where
  liftProgram = liftProgramTrans
