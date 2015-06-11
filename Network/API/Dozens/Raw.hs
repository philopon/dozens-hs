{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Network.API.Dozens.Raw
    ( -- * types
      User, Key, ZoneName, MailAddress
    , DozensException(..)

      -- * authorize
    , Auth(..)
    , Token
    , authorize', authorize

      -- * zone
    , ZoneId
    , Zone(..)

      -- ** get
    , getZone

      -- ** create
    , CreateZone(..)
    , createZone

      -- ** update
    , UpdateZone(..)
    , updateZone

      -- ** delete
    , deleteZone
    
      -- * record
    , RecordType(..)
    , RecordId
    , Record(..)

      -- ** get
    , getRecords

      -- ** create
    , CreateRecord(..)
    , createRecord

      -- ** update
    , UpdateRecord(..)
    , updateRecord

      -- ** delete
    , deleteRecord
    ) where

import Control.Monad(join)
import Control.Exception(Exception, throwIO, catch)
import Control.Concurrent(MVar, newMVar, withMVar, modifyMVar_)

import Network.HTTP.Types.Status(statusCode)
import Network.HTTP.Client

import Text.Read(readEither)
import Data.Word(Word16)
import Data.Typeable(Typeable)
import Data.Aeson(eitherDecode, encode)
import Data.Aeson.Types(FromJSON(..), ToJSON(..), parseEither, (.:), Value(..), object, (.=))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC

type User = S.ByteString
type Key  = S.ByteString

type ZoneName = S.ByteString

data Auth = Auth
    { authUser :: User
    , authKey  :: Key
    , apiBase  :: Request
    } deriving Show

data Token = Token
    { tokenBody   :: MVar S.ByteString
    , tokenAuth   :: Auth
    }

data DozensException
    = AesonParseFailed String
    deriving(Show, Typeable)

instance Exception DozensException

authorize'' :: Auth -> Manager -> IO S.ByteString
authorize'' auth mgr = getToken =<< httpLbs request mgr
  where
    getToken =
        either (throwIO . AesonParseFailed) (return . T.encodeUtf8)
        . join
        . fmap (parseEither (.: "auth_token"))
        . eitherDecode
        . responseBody

    request = (apiBase auth)
        { path = "/api/authorize.json"
        , requestHeaders =
            [ ("X-Auth-User", authUser auth)
            , ("X-Auth-Key",  authKey auth)
            ]
        }

authorize' :: Auth -> Manager -> IO Token
authorize' auth mgr =
    Token
    <$> (newMVar =<< authorize'' auth mgr)
    <*> pure auth

authorize :: User -> Key -> Manager -> IO Token
authorize user key mgr = do
    base <- parseUrl "https://dozens.jp/api"
    authorize'
        Auth { authUser = user
             , authKey  = key
             , apiBase  = base
             } mgr

rawApi' :: FromJSON a => Bool -> (Request -> Request) -> Token -> Manager -> IO a
rawApi' retry f tok mgr = handle401 retry $ withMVar (tokenBody tok) $ \tokBdy ->
    either (throwIO . AesonParseFailed) return
    . eitherDecode
    . responseBody =<< httpLbs (f . authToken tokBdy . apiBase $ tokenAuth tok) mgr

  where
    authToken tokBdy req = req
        { requestHeaders =
            ("X-Auth-Token", tokBdy) :
            ("Content-Type", "application/json") :
            requestHeaders req
        }

    handle401 False io = io
    handle401 True  io = io `catch` \case
        StatusCodeException (statusCode -> 401) _ _ ->
            reAuth >> rawApi' False f tok mgr
        other -> throwIO other

    reAuth = modifyMVar_ (tokenBody tok) $ \_ ->
        authorize'' (tokenAuth tok) mgr

rawApi :: FromJSON a => (Request -> Request) -> Token -> Manager -> IO a
rawApi = rawApi' True

newtype ZoneId = ZoneId Int
    deriving (Show, Eq, Read, Typeable)

instance FromJSON ZoneId where
    parseJSON (String s) = either fail return $ readEither (T.unpack s)
    parseJSON _ = fail "ZoneId: not string"

unZoneId :: ZoneId -> Int
unZoneId (ZoneId z) = z

data Zone = Zone { zoneId :: ZoneId, zoneName :: ZoneName }
    deriving (Show, Eq, Read, Typeable)

instance FromJSON Zone where
    parseJSON (Object o) = Zone
        <$> o .: "id"
        <*> (T.encodeUtf8 <$> o .: "name")
    parseJSON _ = fail "Zone: not object"

newtype Domain = Domain { unDomain :: [Zone] } deriving Show

instance FromJSON Domain where
    parseJSON (Object o) = Domain <$> o .: "domain"
    parseJSON _ = fail "Domain: not object"

retZone :: Functor f => (tok -> mgr -> f Domain) -> tok -> mgr -> f [Zone]
retZone f tok mgr = unDomain <$> f tok mgr

getZone :: Token -> Manager -> IO [Zone]
getZone = retZone $ rawApi $ \r -> r {path = "/api/zone.json"}

type MailAddress = S.ByteString

data CreateZone = CreateZone
    { newZoneName         :: ZoneName
    , googleAppsAuthorize :: Maybe S.ByteString
    , mailAddress         :: Maybe MailAddress
    } deriving (Show, Read, Eq, Typeable)

instance ToJSON CreateZone where
    toJSON CreateZone{..} = object $
        maybe id (\m -> (:) ("mailaddress" .= T.decodeUtf8 m)) mailAddress $
        maybe id (\g -> (:) ("google_authorize" .= T.decodeUtf8 g)) googleAppsAuthorize $
        "name" .= T.decodeUtf8 newZoneName :
        ["add_google_apps" .= maybe False (const True) googleAppsAuthorize]

rawPostApi :: (ToJSON d, FromJSON a) => d -> (Request -> Request) -> Token -> Manager -> IO a
rawPostApi d f = rawApi $ \r -> f $ r
    { method = "POST"
    , requestBody = RequestBodyLBS $ encode d
    }

createZone :: CreateZone -> Token -> Manager -> IO [Zone]
createZone cz = retZone $ rawPostApi cz $ \r ->
    r { path   = "/api/zone/create.json" }

data UpdateZone = UpdateZone
    { updateZoneId      :: ZoneId
    , updateMailAddress :: MailAddress
    } deriving (Show, Read, Eq, Typeable)

updateZone :: UpdateZone -> Token -> Manager -> IO [Zone]
updateZone z = retZone $ rawPostApi d $ \r -> r
    { path   = S.concat
        [ "/api/zone/update/"
        , (SC.pack . show . unZoneId . updateZoneId) z
        , ".json"
        ]
    }
  where
    d = object ["mailaddress" .= T.decodeUtf8 (updateMailAddress z)]

deleteZone :: ZoneId -> Token -> Manager -> IO [Zone]
deleteZone z = retZone $ rawApi $ \r -> r
    { method = "DELETE"
    , path = S.concat
        [ "/api/zone/delete/"
        , (SC.pack . show . unZoneId) z
        , ".json"
        ]
    }

data RecordType = A | AAAA | CNAME | MX | TXT
    deriving (Show, Eq, Read, Typeable)

instance FromJSON RecordType where
    parseJSON (String s) = either fail return $ readEither (T.unpack s)
    parseJSON _ = fail "RecordType: not string"

instance ToJSON RecordType where
    toJSON A     = "A"
    toJSON AAAA  = "AAAA"
    toJSON CNAME = "CNAME"
    toJSON MX    = "MX"
    toJSON TXT   = "TXT"

newtype RecordId = RecordId Int
    deriving (Show, Eq, Read, Typeable)

unRecordId :: RecordId -> Int
unRecordId (RecordId i) = i

instance FromJSON RecordId where
    parseJSON (String s) = either fail (return . RecordId) $ readEither (T.unpack s)
    parseJSON _ = fail "RecordId: not string"

data Record = Record
    { recordId       :: RecordId
    , recordName     :: S.ByteString
    , recordType     :: RecordType
    , recordPriority :: Maybe Word16
    , recordTtl      :: Int
    , recordBody     :: S.ByteString
    } deriving (Show, Eq, Read, Typeable)

instance FromJSON Record where
    parseJSON (Object o) = Record
        <$> o .: "id"
        <*> (T.encodeUtf8 <$> o .: "name")
        <*> o .: "type"
        <*> (o .: "prio" >>= parsePriority)
        <*> (o .: "ttl" >>= either fail return . readEither)
        <*> (T.encodeUtf8 <$> o .: "content")
      where
        parsePriority Null = return Nothing
        parsePriority (String s) = either fail (return . Just) $ readEither (T.unpack s)
        parsePriority _ = fail "not string"
    parseJSON _ = fail "Record: not object"

newtype Records = Records { unRecords :: [Record] }

instance FromJSON Records where
    parseJSON (Object o) = Records <$> o .: "record"
    parseJSON _ = fail "Records: not object"

retRecords :: Functor f => (tok -> mgr -> f Records) -> tok -> mgr -> f [Record]
retRecords f tok mgr = unRecords <$> f tok mgr

getRecords :: ZoneName -> Token -> Manager -> IO [Record]
getRecords zn = retRecords $ rawApi $ \r -> r
    { path = S.concat
        [ "/api/record/"
        , zn
        , ".json"
        ]
    }

data CreateRecord = CreateRecord
    { crDomain   :: ZoneName
    , crName     :: S.ByteString
    , crType     :: RecordType
    , crPriority :: Maybe Word16
    , crTtl      :: Maybe Int
    , crBody     :: S.ByteString
    } deriving (Show, Eq, Read, Typeable)

instance ToJSON CreateRecord where
    toJSON CreateRecord{..} = object $
        maybe id (\p -> (:) ("prio" .= p)) crPriority $
        maybe id (\t -> (:) ("ttl"  .= t)) crTtl $
        [ "domain"  .= T.decodeUtf8 crDomain
        , "name"    .= T.decodeUtf8 crName
        , "type"    .= crType
        , "content" .= T.decodeUtf8 crBody
        ]

createRecord :: CreateRecord -> Token -> Manager -> IO [Record]
createRecord cr = retRecords $ rawPostApi cr $ \r -> r
    { path = "/api/record/create.json" }

data UpdateRecord = UpdateRecord
    { urRecordId :: RecordId
    , urPriority :: Maybe Word16
    , urTtl      :: Maybe Int
    , urBody     :: Maybe S.ByteString
    } deriving (Show, Eq, Read, Typeable)

newtype UpdateRecord' = UpdateRecord' UpdateRecord

instance ToJSON UpdateRecord' where
    toJSON (UpdateRecord' UpdateRecord{..}) = object $
        maybe id (\p -> (:) ("prio"    .= p)) urPriority $
        maybe id (\b -> (:) ("content" .= T.decodeUtf8 b)) urBody $
        maybe id (\t -> (:) ("ttl"     .= t)) urTtl $
        []

updateRecord :: UpdateRecord -> Token -> Manager -> IO [Record]
updateRecord ur = retRecords $ rawPostApi (UpdateRecord' ur) $ \r -> r
    { path = S.concat
        [ "/api/record/update/"
        , (SC.pack . show . unRecordId . urRecordId) ur
        , ".json"
        ]
    }

deleteRecord :: RecordId -> Token -> Manager -> IO [Record]
deleteRecord rid = retRecords $ rawApi $ \r -> r
    { method = "DELETE"
    , path   = S.concat
        [ "/api/record/delete/"
        , (SC.pack . show . unRecordId) rid
        , ".json"
        ]
    }
