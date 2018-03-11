{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Tola.Database.Model where

import           Control.Monad.IO.Class              (MonadIO (..), liftIO)
import           Control.Monad.Reader                (asks)
import           Control.Monad.Reader.Class          (MonadReader)
import           Control.Monad.Trans.Class           (MonadTrans, lift)
import           Control.Monad.Trans.Control         (MonadBaseControl)
import           Control.Monad.Trans.Reader          (ReaderT (..))
import qualified Data.Aeson                          as A
import qualified Data.ByteString.Lazy                as BL
import           Data.Pool                           (Pool)
import           Data.Text                           (Text)
import qualified Data.Text.Encoding                  as E
import qualified Data.Time                           as Time
import qualified Data.Time.Clock.POSIX               as POSIX
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import qualified Tola.Types.ChargeRequest            as ChargeRequest
import qualified Tola.Types.ChargeResponse           as ChargeResponse
import           Tola.Types.Common
import qualified Tola.Types.DisbursementNotification as DisbursementNotification
import qualified Tola.Types.LodgementNotification    as LodgementNotification


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DBLodgementNotification sql=lodgement_notifications json
  Id
  creationTime Time.UTCTime default=now() MigrationOnly
  amount Amount sqltype=numeric(14,5)
  msisdn Msisdn
  reference SourceReference
  customerReference CustomerReference
  operatorReference OperatorReference
  sourceReference ArbitraryReference
  date Time.UTCTime
  rawNotification Text sqltype=json

DBDisbursementNotification sql=disbursement_notifications json
  Id
  creationTime Time.UTCTime default=now() MigrationOnly
  success Bool
  errorMessage Text Maybe
  amount Amount sqltype=numeric(14,5)
  msisdn Msisdn
  customerReference ArbitraryReference
  operatorReference OperatorReference
  sourceReference SourceReference
  date Time.UTCTime
  rawNotification Text sqltype=json

DBChargeRequest sql=charge_request json
  Id
  creationTime Time.UTCTime default=now() MigrationOnly
  lodgementNotificationId DBLodgementNotificationId Maybe
  disbursementNotificationId DBDisbursementNotificationId Maybe
  amount Amount sqltype=numeric(14,5)
  msisdn Msisdn
  state ChargeRequest.ChargeRequestState sqltype=chargerequeststate
  reference SourceReference Maybe
  responseErrorCode Int Maybe
  responseErrorMessage Text Maybe
  rawResponse Text Maybe sqltype=json
|]
