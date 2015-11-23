{-# LANGUAGE DeriveDataTypeable #-}
module Yandex.Direct.Types (Token, Method, YDException(..)) where


import Data.Text (Text)
import Data.Typeable (Typeable)
import Control.Exception (Exception)

type Token = Text
type Method = Text

data YDException =
    OtherYDError
    deriving (Typeable,Show,Eq)

instance Exception YDException
