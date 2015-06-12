dozens-hs
==
[![Build Status](https://travis-ci.org/philopon/dozens-hs.svg?branch=master)](https://travis-ci.org/philopon/dozens-hs)
[![Hackage](http://img.shields.io/hackage/v/dozens.svg)](https://hackage.haskell.org/package/dozens)
[![MIT license](http://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

dozens(https://dozens.jp/) api library for haskell

```.hs
{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.API.Dozens

user :: Auth
user = def
    { authUser = "Your User Name"
    , authKey  = "Your API Key"
    }

main :: IO ()
main = withManager tlsManagerSettings $ withDozens user $ do
    run getZone >>= print

    z  <- run createZone { czZoneName = "dummy-dozens.jp" }

    rs <- run createRecord
        { crDomain   = "dummy-dozens.jp"
        , crName     = "www"
        , crType     = A
        , crPriority = Just 10
        , crBody     = "192.168.1.10"
        , crTtl      = Just 7200
        }

    run updateRecord
        { urRecordId = recordId $ head rs
        , urPriority = Just 50
        }

    run $ DeleteRecord (recordId $ head rs)

    run $ DeleteZone (zoneId . head $ filter (("dummy-dozens.jp" ==) . zoneName) z)
    return ()
```

My invitation code
==
http://dozens.jp/i/nJQCiO
