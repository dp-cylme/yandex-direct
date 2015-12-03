{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module Yandex.Direct.Base (ydpost) where

import           Control.Lens        ((^.))
import           Data.Aeson          (FromJSON, FromJSON, ToJSON,
                                      Value (Object), object, parseJSON, toJSON,
                                      (.:), (.=))

import           Control.Applicative ((<|>))
import           Control.Exception   (throwIO)
import           Control.Monad       (mzero)
import           Data.Aeson.Lens     (key)
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           Network.Wreq        (Response, asJSON, post, responseBody)

import           Debug.Trace
import           Yandex.Direct.Types


data GenYDReq a = GenYDReq
    { method :: Method
    , param  :: Maybe a
    , locale :: Text
    , token  :: Token
    } deriving (Generic)

instance ToJSON a => ToJSON (GenYDReq a)

data GenYDRep a
    = GenYDRep { _data :: a}
    | GenYDErr { error_code :: Int
               , error_str :: Text
               , error_detail :: Text}
    deriving (Show)

instance FromJSON a => FromJSON (GenYDRep a) where
    parseJSON (Object v) =
        (GenYDRep <$> v .: "data") <|>
        (GenYDErr <$> v .: "error_code" <*> v .: "error_str" <*>
         v .: "error_detail")
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
             }) >>= asJSON >>= extractData
  where
    extractData
        :: (Show b)
        => Response (GenYDRep b) -> IO b
    extractData r =
        case r ^. responseBody of
            (GenYDRep d) -> return d
            (GenYDErr code str detail) -> throwIO $ ydException code str detail
