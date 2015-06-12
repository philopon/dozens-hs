module Network.API.Dozens.Common
    ( Auth(..)
    , Token

      -- * zone
    , ZoneId
    , Zone(..)

      -- * record
    , RecordId
    , RecordType(..)
    , Record(..)

      -- * aliases
    , User, Key, ZoneName, MailAddress

    , def
    ) where

import Network.API.Dozens.Internal
import Data.Default.Class
