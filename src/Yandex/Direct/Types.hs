{-# LANGUAGE DeriveDataTypeable #-}
module Yandex.Direct.Types (Token, Method, YDException(..), ydException) where


import Data.Text (Text)
import Data.Typeable (Typeable)
import Control.Exception (Exception)

type Token = Text
type Method = Text

data YDException
    = YDNotEnoughPoint Text
                       Text
    | YDTooManyReportsInQueue Text
                              Text
    | YDReportNotReady Text
                       Text
    | YDAuthError Text
                  Text
    | OtherYDError Int
                   Text
                   Text
    deriving (Typeable,Show,Eq)

instance Exception YDException


ydException :: Int -> Text -> Text -> YDException
ydException code str detail = case code of
                                152 -> YDNotEnoughPoint str detail
                                31 -> YDTooManyReportsInQueue str detail
                                92 -> YDReportNotReady str detail
                                53 -> YDAuthError str detail
                                _ -> OtherYDError code str detail
