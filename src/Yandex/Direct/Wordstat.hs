{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Yandex.Direct.Wordstat
       (createNew, delete, get, getList, WordstatReportId,
        WordstatReportDeleteStatus(..), WordstatStatusReport(..),
        WordstatReportInfo(..))
       where

import GHC.Generics (Generic)
import Data.Aeson
       (FromJSON, ToJSON, (.:), object, toJSON, (.=), parseJSON,
        Value(..), decode)
import Data.Text (Text)
import Control.Monad (mzero)
import Yandex.Direct.Base
import Yandex.Direct.Types (Token)

type Phrase = Text
type GeoId = Text
type WordstatReportId = Int

data WordstatReportDeleteStatus
    = Success
    | Failure
    deriving (Show)

instance FromJSON WordstatReportDeleteStatus where
    parseJSON (Number v) =
        case v of
            1 -> return Success
            _ -> return Failure
    parseJSON _ = return Failure

data WordstatStatusReport
    = Done
    | Pending
    | Deleted
    deriving (Generic, Show)

instance FromJSON WordstatStatusReport

data WordstatItem = WI
    { wiPrases :: Phrase
    , wiShows :: Int
    } deriving (Show)

instance FromJSON WordstatItem where
    parseJSON (Object v) = WI <$> v .: "Phrase" <*> v .: "Shows"
    parseJSON _ = mzero

data WordstatReportInfo = WRI
    { wriPhrase :: Phrase
    , wriGeoid :: [GeoId]
    , wriSearchedWith :: [WordstatItem]
    , wriSearchedAlso :: [WordstatItem]
    } deriving (Show)

instance FromJSON WordstatReportInfo where
    parseJSON (Object v) =
        WRI <$> v .: "Phrase" <*> v .: "GeoID" <*> v .: "SearchedWith" <*>
        v .: "SearchedAlso"
    parseJSON _ = mzero

data WordstatReportStatusInfo = WRSI
    { wrsiReportId :: WordstatReportId
    , wrsiStatusReport :: WordstatStatusReport
    } deriving (Show)

instance FromJSON WordstatReportStatusInfo where
    parseJSON (Object v) = WRSI <$> v .: "ReportID" <*> v .: "StatusReport"
    parseJSON _ = mzero

data NewWordstatReportInfo = NWRI
    { nwriPhrases :: [Phrase]
    , nwriGeoID   :: [GeoId]
    }


instance ToJSON NewWordstatReportInfo where
    toJSON (NWRI phrases geoids) =
        object ["Phrases" .= phrases, "GeoID" .= geoids]

createNew :: Token -> [Phrase] -> [GeoId] -> IO WordstatReportId
createNew token phrases geoids =
    ydpost
        "CreateNewWordstatReport"
        (Just
             NWRI
             { nwriPhrases = phrases
             , nwriGeoID = geoids
             })
        token


delete :: Token -> WordstatReportId -> IO WordstatReportDeleteStatus
delete token reportId = ydpost "DeleteWordstatReport" (Just reportId) token


get :: Token -> WordstatReportId -> IO [WordstatReportInfo]
get token reportId = ydpost "GetWordstatReport" (Just reportId) token


getList :: Token -> IO [WordstatReportStatusInfo]
getList = ydpost "GetWordstatReportList" nothing
  where
    nothing :: Maybe Text
    nothing = Nothing
