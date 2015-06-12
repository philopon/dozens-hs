{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Network.API.Dozens.Internal
    ( -- * types
      User, Key, ZoneName, MailAddress
    , DozensException(..)

      -- * authorize
    , Auth(..)
    , Token
    , authorize
    , fromToken

      -- * zone
    , ZoneId(..)
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
    , DeleteZone(..)
    , deleteZone
    
      -- * record
    , RecordType(..)
    , RecordId(..)
    , Record(..)

      -- ** get
    , GetRecords(..)
    , getRecords

      -- ** create
    , CreateRecord(..)
    , createRecord

      -- ** update
    , UpdateRecord(..)
    , updateRecord

      -- ** delete
    , DeleteRecord(..)
    , deleteRecord
    ) where

import Control.Monad(join)
import Control.Exception(Exception, throwIO, catch)
import Control.Concurrent(MVar, newMVar, withMVar, modifyMVar_)

import Network.HTTP.Types.Status(statusCode)
import Network.HTTP.Client

import Text.Read(readMaybe)

import Data.Scientific
import Data.Default.Class(Default(..))
import Data.Word(Word16)
import Data.Typeable(Typeable)
import Data.Aeson(eitherDecode, encode)
import Data.Aeson.Types(FromJSON(..), ToJSON(..), parseEither, (.:), Value(..), object, (.=), Parser)
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

instance Default Auth where
    def = Auth "USER" "KEY" "https://dozens.jp"

data Token = Token
    { tokenBody   :: MVar S.ByteString
    , tokenAuth   :: Auth
    }

data DozensException
    = AesonParseFailed String
    deriving(Show, Typeable)

instance Exception DozensException

authorize' :: Auth -> Manager -> IO S.ByteString
authorize' auth mgr = getToken =<< httpLbs request mgr
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

fromToken :: Auth -> S.ByteString -> IO Token
fromToken auth tok = Token <$> (newMVar tok) <*> pure auth

authorize :: Auth -> Manager -> IO Token
authorize auth mgr = authorize' auth mgr >>= \tok -> fromToken auth tok

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
        authorize' (tokenAuth tok) mgr

rawApi :: FromJSON a => (Request -> Request) -> Token -> Manager -> IO a
rawApi = rawApi' True

jsonInt :: (Integral i, Read i) => Value -> Parser i
jsonInt (String s) =
    maybe (fail $ "cannot read JSON string as Int: " ++ show s) return
    . readMaybe $ T.unpack s
jsonInt (Number n) = either (fail . ("cannot read JSON floating as Int: " ++) . show :: Double -> Parser a) return (floatingOrInteger n)
jsonInt a = fail $ "cannot read JSON as Int: " ++ show a

newtype ZoneId = ZoneId Int
    deriving (Show, Eq, Read, Typeable)

newtype ZoneId' = ZoneId' { unZoneId' :: ZoneId }

instance FromJSON ZoneId' where
    parseJSON = fmap (ZoneId' . ZoneId) . jsonInt

unZoneId :: ZoneId -> Int
unZoneId (ZoneId z) = z

data Zone = Zone { zoneId :: ZoneId, zoneName :: ZoneName }
    deriving (Show, Eq, Read, Typeable)

newtype Zone' = Zone' { unZone' :: Zone }

instance FromJSON Zone' where
    parseJSON (Object o) = fmap Zone' $ Zone
        <$> (unZoneId' <$> o .: "id")
        <*> (T.encodeUtf8 <$> o .: "name")
    parseJSON _ = fail "Zone: not object"

newtype Domain = Domain { unDomain :: [Zone] } deriving Show

instance FromJSON Domain where
    parseJSON (Object o) = Domain . map unZone' <$> o .: "domain"
    parseJSON _ = fail "Domain: not object"

retZone :: Functor f => (tok -> mgr -> f Domain) -> tok -> mgr -> f [Zone]
retZone f tok mgr = unDomain <$> f tok mgr

getZone :: Token -> Manager -> IO [Zone]
getZone = retZone $ rawApi $ \r -> r {path = "/api/zone.json"}

type MailAddress = S.ByteString

data CreateZone = CreateZone
    { czZoneName            :: ZoneName
    , czGoogleAppsAuthorize :: Maybe S.ByteString
    , czMailAddress         :: Maybe MailAddress
    } deriving (Show, Read, Eq, Typeable)

newtype CreateZone' = CreateZone' CreateZone

instance ToJSON CreateZone' where
    toJSON (CreateZone' CreateZone{..}) = object $
        maybe id (\m -> (:) ("mailaddress" .= T.decodeUtf8 m)) czMailAddress $
        maybe id (\g -> (:) ("google_authorize" .= T.decodeUtf8 g)) czGoogleAppsAuthorize $
        "name" .= T.decodeUtf8 czZoneName :
        ["add_google_apps" .= maybe False (const True) czGoogleAppsAuthorize]

rawPostApi :: (ToJSON d, FromJSON a) => d -> (Request -> Request) -> Token -> Manager -> IO a
rawPostApi d f = rawApi $ \r -> f $ r
    { method = "POST"
    , requestBody = RequestBodyLBS $ encode d
    }

createZone :: CreateZone -> Token -> Manager -> IO [Zone]
createZone cz = retZone $ rawPostApi (CreateZone' cz) $ \r ->
    r { path   = "/api/zone/create.json" }

data UpdateZone = UpdateZone
    { uzZoneId      :: ZoneId
    , uzMailAddress :: MailAddress
    } deriving (Show, Read, Eq, Typeable)

updateZone :: UpdateZone -> Token -> Manager -> IO [Zone]
updateZone z = retZone $ rawPostApi d $ \r -> r
    { path   = S.concat
        [ "/api/zone/update/"
        , (SC.pack . show . unZoneId . uzZoneId) z
        , ".json"
        ]
    }
  where
    d = object ["mailaddress" .= T.decodeUtf8 (uzMailAddress z)]

newtype DeleteZone = DeleteZone { dzZoneId :: ZoneId }
    deriving (Show, Read, Eq)

deleteZone :: DeleteZone -> Token -> Manager -> IO [Zone]
deleteZone (DeleteZone z) = retZone $ rawApi $ \r -> r
    { method = "DELETE"
    , path = S.concat
        [ "/api/zone/delete/"
        , (SC.pack . show . unZoneId) z
        , ".json"
        ]
    }

data RecordType = A | AAAA | CNAME | MX | TXT
    deriving (Show, Eq, Read, Typeable)

newtype RecordType' = RecordType' { unRecordType' :: RecordType }

instance FromJSON RecordType' where
    parseJSON (String t) = RecordType' <$> case t of
        "A"     -> return A
        "AAAA"  -> return AAAA
        "CNAME" -> return CNAME
        "MX"    -> return MX
        "TXT"   -> return TXT
        o       -> fail $ "unknown RecordType: " ++ show o
    parseJSON _ = fail "RecordType: not string"

instance ToJSON RecordType' where
    toJSON t = case unRecordType' t of
        A     -> "A"
        AAAA  -> "AAAA"
        CNAME -> "CNAME"
        MX    -> "MX"
        TXT   -> "TXT"

newtype RecordId = RecordId Int
    deriving (Show, Eq, Read, Typeable)

unRecordId :: RecordId -> Int
unRecordId (RecordId i) = i

newtype RecordId' = RecordId' { unRecordId' :: RecordId }

instance FromJSON RecordId' where
    parseJSON = fmap (RecordId' . RecordId) . jsonInt

data Record = Record
    { recordId       :: RecordId
    , recordName     :: S.ByteString
    , recordType     :: RecordType
    , recordPriority :: Maybe Word16
    , recordTtl      :: Int
    , recordBody     :: S.ByteString
    } deriving (Show, Eq, Read, Typeable)

newtype Record' = Record' { unRecord' :: Record }

instance FromJSON Record' where
    parseJSON (Object o) = fmap Record' $ Record
        <$> (unRecordId' <$> o .: "id")
        <*> (T.encodeUtf8 <$> o .: "name")
        <*> (unRecordType' <$> o .: "type")
        <*> (o .: "prio" >>= parsePriority)
        <*> (o .: "ttl" >>= jsonInt)
        <*> (T.encodeUtf8 <$> o .: "content")
      where
        parsePriority Null = return Nothing
        parsePriority j    = Just <$> jsonInt j
    parseJSON _ = fail "Record: not object"

newtype Records = Records { unRecords :: [Record] }

instance FromJSON Records where
    parseJSON (Object o) = Records . map unRecord' <$> o .: "record"
    parseJSON (Array  _) = return $ Records []
    parseJSON o = fail $ "Records: not object: " ++ show o

retRecords :: Functor f => (tok -> mgr -> f Records) -> tok -> mgr -> f [Record]
retRecords f tok mgr = unRecords <$> f tok mgr

newtype GetRecords = GetRecords { grZoneName :: ZoneName }
    deriving (Show, Read, Eq)

getRecords :: GetRecords -> Token -> Manager -> IO [Record]
getRecords (GetRecords zn) = retRecords $ rawApi $ \r -> r
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

newtype CreateRecord' = CreateRecord' CreateRecord

instance ToJSON CreateRecord' where
    toJSON (CreateRecord' CreateRecord{..}) = object $
        maybe id (\p -> (:) ("prio" .= p)) crPriority $
        maybe id (\t -> (:) ("ttl"  .= t)) crTtl $
        [ "domain"  .= T.decodeUtf8 crDomain
        , "name"    .= T.decodeUtf8 crName
        , "type"    .= RecordType' crType
        , "content" .= T.decodeUtf8 crBody
        ]

createRecord :: CreateRecord -> Token -> Manager -> IO [Record]
createRecord cr = retRecords $ rawPostApi (CreateRecord' cr) $ \r -> r
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

newtype DeleteRecord = DeleteRecord { drRecordId :: RecordId }
    deriving (Show, Read, Eq)

deleteRecord :: DeleteRecord -> Token -> Manager -> IO [Record]
deleteRecord (DeleteRecord rid) = retRecords $ rawApi $ \r -> r
    { method = "DELETE"
    , path   = S.concat
        [ "/api/record/delete/"
        , (SC.pack . show . unRecordId) rid
        , ".json"
        ]
    }
