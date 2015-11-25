{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text
import Control.Concurrent (threadDelay)
import Yandex

main :: IO ()
main = do
    let token = "{your-token}" :: Text
        phrases = ["пух -винни", "синтепон"] :: [Text]
        geoids = ["213"]
    reportId <- createNew token phrases geoids
    print reportId
    l <- getList token
    print l
    threadDelay 90000000
    g <- get token reportId
    print g
    d <- delete token reportId
    print d
