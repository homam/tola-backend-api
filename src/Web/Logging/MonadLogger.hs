{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs             #-}

module Web.Logging.MonadLogger where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.RWS
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import qualified Data.ByteString.Char8      as Char8

class Monad m => MonadLogger m where
  writeLog :: Char8.ByteString -> m ()

  default writeLog :: (MonadTrans t, MonadLogger m', m ~ t m') => Char8.ByteString -> m ()
  writeLog = lift . writeLog

--

instance MonadLogger m => MonadLogger (ReaderT r m)
instance (MonadLogger m, Monoid w) => MonadLogger (WriterT w m)
instance MonadLogger m => MonadLogger (StateT s m)
instance (MonadLogger m, Monoid w) => MonadLogger (RWST r w s m)
instance MonadLogger m => MonadLogger (ExceptT e m)
instance MonadLogger m => MonadLogger (ContT r m)
