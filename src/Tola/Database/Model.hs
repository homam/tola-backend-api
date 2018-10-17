{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Tola.Database.Model where

import           Data.Text                        (Text)
import qualified Data.Time                        as Time
import           Database.Persist.Postgresql.Json
import           Database.Persist.TH
import qualified Tola.Types.ChargeRequest         as ChargeRequest
import           Tola.Types.Common


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
  campaignId DBCampaignId Maybe
  queryString Json Maybe

DBPixelTemplate sql=pixel_templates
  Id
  creationTime Time.UTCTime default=now() MigrationOnly
  url Text

DBPixel sql=pixels json
  Id
  creationTime Time.UTCTime default=now() MigrationOnly
  chargeRequestId DBChargeRequestId
  url Url
  success Bool Maybe
  responseStatusCode Int Maybe
  response Text Maybe
  responseHeaders Json Maybe
  pixelAmount Amount sqltype=numeric(14,5) Maybe


DBCampaign sql=campaigns json
  Id
  creationTime Time.UTCTime default=now() MigrationOnly
  name Text
  isActive Bool
  ouisysCampaignId OuiSysCampaignId
  pixelTemplateId DBPixelTemplateId Maybe
  UniqueOuiSysCampaignId ouisysCampaignId
|]