{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Web.Types.WebApp where

import           Control.Monad.Reader
import qualified Data.ByteString.Char8   as Char8
import qualified Data.Text.Lazy          as TL
import qualified Data.Vault.Lazy         as V
import qualified Network.Wai             as W
import           Tola.MonadTolaApi
import           Web.Logging.MonadLogger
import           Web.Scotty.Trans
import           Web.Types.State


type WebApp = forall (t :: * -> *)
    . (
       MonadLogger (ActionT TL.Text t)
     , MonadTolaApi (ActionT TL.Text t)
     , MonadIO t
     )
  => ScottyT TL.Text t ()


writeLog' :: forall (m :: * -> *) e
   . (MonadIO m, MonadReader AppState m, ScottyError e)
  => Char8.ByteString
  -> ActionT e m ()
writeLog' str = do
  req <- request
  key <- lift $ asks appVaultLoggerKey
  let logger = V.lookup key (W.vault req)
  liftIO $ case logger of
    Nothing      -> return ()
    Just logger' -> logger' str

