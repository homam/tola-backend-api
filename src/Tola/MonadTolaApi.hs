{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs             #-}

module Tola.MonadTolaApi where

import           Control.Monad.Trans.Class
import           Tola.Types.ChargeRequest
import           Tola.Types.ChargeResponse
--
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.RWS
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
--
import           Tola.Types.Common

type ResponseWithError a = Either String a

class Monad m => MonadTolaApi m where
  makeChargeRequest :: ChargeRequest -> m ChargeResponse

  default makeChargeRequest :: (MonadTrans t, MonadTolaApi m', m ~ t m') => ChargeRequest -> m ChargeResponse
  makeChargeRequest = lift . makeChargeRequest


instance MonadTolaApi m => MonadTolaApi (ReaderT r m)
instance (MonadTolaApi m, Monoid w) => MonadTolaApi (WriterT w m)
instance MonadTolaApi m => MonadTolaApi (StateT s m)
instance (MonadTolaApi m, Monoid w) => MonadTolaApi (RWST r w s m)
instance MonadTolaApi m => MonadTolaApi (ExceptT e m)
instance MonadTolaApi m => MonadTolaApi (ContT r m)
