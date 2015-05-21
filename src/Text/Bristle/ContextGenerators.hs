{-# LANGUAGE OverloadedStrings #-}

module Text.Bristle.ContextGenerators where

import Data.Text
import Data.Maybe
import Control.Monad
import Text.Bristle.Types
import System.Environment (lookupEnv)
import Network.HTTP


fromEnv :: Context IO
fromEnv t = do
    mvalue <- lookupEnv $ unpack t
    let value = fromMaybe "" mvalue
    return $ Just $ ContextText $ pack value

fromHttp :: Context IO
fromHttp t = do
    let url = "http://private-1f7e-sgillis.apiary-mock.com/" ++ (unpack t)
    response <- simpleHTTP (getRequest url)
    (x, _, _) <- getResponseCode response
    if x == 2
        then getResponseBody response >>= return . Just . ContextText . pack
        else return Nothing
