{-# LANGUAGE OverloadedStrings #-}

module Network.API.Dozens.API
    ( -- * Zone
      -- ** get
      GetZone, getZone

      -- ** create
    , I.CreateZone(..), createZone

      -- ** update
    , I.UpdateZone(..), updateZone

      -- ** delete
    , I.DeleteZone(..), deleteZone

      -- * Record
      -- ** get
    , I.GetRecords(..), getRecords

      -- ** create
    , I.CreateRecord(..), createRecord

      -- ** update
    , I.UpdateRecord(..), updateRecord

      -- ** delete
    , I.DeleteRecord(..), deleteRecord

      -- * aliases
    , I.User, I.Key, I.ZoneName, I.MailAddress

    ) where

import qualified Network.API.Dozens.Internal as I

data GetZone = GetZone
    deriving (Show, Read, Eq)

getZone :: GetZone
getZone = GetZone

createZone :: I.CreateZone
createZone = I.CreateZone "" Nothing Nothing

updateZone :: I.UpdateZone
updateZone = I.UpdateZone (I.ZoneId 0) ""

deleteZone :: I.DeleteZone
deleteZone = I.DeleteZone (I.ZoneId 0)

getRecords :: I.GetRecords
getRecords = I.GetRecords ""

createRecord :: I.CreateRecord
createRecord = I.CreateRecord "" "" I.A Nothing Nothing ""

updateRecord :: I.UpdateRecord
updateRecord = I.UpdateRecord (I.RecordId 0) Nothing Nothing Nothing

deleteRecord :: I.DeleteRecord
deleteRecord = I.DeleteRecord (I.RecordId 0)


