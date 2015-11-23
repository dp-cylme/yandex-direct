{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Yandex.Direct.Base (ydpost) where

import Control.Lens ((^.))
import Data.Aeson
       (FromJSON, ToJSON, toJSON, object, (.=), parseJSON,
        FromJSON, (.:), Value(Object))

import GHC.Generics (Generic)
import Data.Aeson.Lens (key)
import Data.Text (Text)
import Network.Wreq (asJSON, post, responseBody, Response)
import Control.Exception (throwIO)
import Control.Monad (mzero)

import Yandex.Direct.Types
import Debug.Trace


data GenYDReq a = GenYDReq
    { method :: Method
    , param :: Maybe a
    , locale :: Text
    , token :: Token
    } deriving (Generic)

instance ToJSON a => ToJSON (GenYDReq a)

data GenYDRep a = GenYDRep
    { _data :: a
    } deriving (Show)

instance FromJSON a => FromJSON (GenYDRep a) where
    parseJSON (Object v) = GenYDRep <$> v .: "data"
    parseJSON _ = mzero

ydpost
    :: (ToJSON a, FromJSON b, Show b)
    => Method -> Maybe a -> Token -> IO b
ydpost m p t =
    post
        "https://api-sandbox.direct.yandex.ru/v4/json/"
        (toJSON
             GenYDReq
             { method = m
             , param = p
             , token = t
             , locale = "ru"
             }) >>= \r -> trace (show r) $ 
    asJSON r >>=
    extractData
  where
    extractData :: (Show b) => Response (GenYDRep b) -> IO b
    extractData r = return $ _data (r ^. responseBody)
