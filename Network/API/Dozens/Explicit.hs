{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Network.API.Dozens.Explicit
    ( -- * authorize
      I.authorize
    , I.fromToken

      -- * API
    , API(..)

    , module Network.API.Dozens.API
    , module Network.API.Dozens.Common
    ) where

import Network.HTTP.Client
import qualified Network.API.Dozens.Internal as I

import Network.API.Dozens.Common
import Network.API.Dozens.API

class API a r | a -> r where
    run :: a -> I.Token -> Manager -> IO r

instance API GetZone [Zone] where
    run _ = I.getZone

instance API I.CreateZone [Zone] where
    run = I.createZone

instance API I.UpdateZone [Zone] where
    run = I.updateZone

instance API DeleteZone [Zone] where
    run = I.deleteZone

instance API GetRecords [Record] where
    run = I.getRecords

instance API I.CreateRecord [Record] where
    run = I.createRecord

instance API I.UpdateRecord [Record] where
    run = I.updateRecord

instance API DeleteRecord [Record] where
    run = I.deleteRecord
