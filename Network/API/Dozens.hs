{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Network.API.Dozens
    ( Dozens, withDozens
    , API(..)

    , module Network.API.Dozens.API
    , module Network.API.Dozens.Common
    ) where


import Control.Monad.IO.Class
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative((<$>), (<*>), pure)
#endif

import Network.HTTP.Client
import qualified Network.API.Dozens.Internal as I

import Network.API.Dozens.API
import Network.API.Dozens.Common

import Data.Reflection

data Dozens = Dozens
    { dozensToken   :: I.Token
    , dozensManager :: Manager
    }

withDozens :: MonadIO m => I.Auth -> (Given Dozens => m a) -> Manager -> m a
withDozens auth m mgr = do
    doz <- liftIO $ Dozens <$> I.authorize auth mgr <*> pure mgr
    give doz m

class API a r | a -> r where
    run :: (MonadIO m, Given Dozens) => a -> m r

wrap :: (MonadIO m, Given Dozens) => (a -> I.Token -> Manager -> IO r) -> a -> m r
wrap f a = liftIO $ f a (dozensToken given) (dozensManager given)

instance API GetZone [Zone] where
    run = wrap $ const I.getZone

instance API CreateZone [Zone] where
    run = wrap I.createZone

instance API UpdateZone [Zone] where
    run = wrap I.updateZone

instance API DeleteZone [Zone] where
    run = wrap I.deleteZone

instance API GetRecords [Record] where
    run = wrap I.getRecords

instance API CreateRecord [Record] where
    run = wrap I.createRecord

instance API UpdateRecord [Record] where
    run = wrap I.updateRecord

instance API DeleteRecord [Record] where
    run = wrap I.deleteRecord
