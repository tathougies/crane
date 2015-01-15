{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Web.Crane.Wai
    ( W.HostPreference(..)
    , CraneSettings (..)

    , craneRunDefault
    , craneRun

    , defaultSettings

    , waiAppForApp) where

import Web.Crane.Types
import Web.Crane.Routes
import Web.Crane.Site

import Control.Monad.Reader
import Control.Monad.Error

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import Data.String
import Data.Monoid

import Network.Wai
import qualified Network.Wai.Handler.Warp as W

import Network.HTTP.Types.Status

data CraneSettings = CraneSettings
                   { craneServerPort :: W.Port
                   , craneServerHost :: W.HostPreference }

defaultSettings :: CraneSettings
defaultSettings = CraneSettings 8000 "*"

craneRunDefault :: CraneApp a => a -> IO ()
craneRunDefault app = craneRun defaultSettings (defaultSiteSpec app)

craneRun :: CraneApp a => CraneSettings -> CraneSiteSpec a -> IO ()
craneRun settings spec = do
  (site, app) <- mkSite spec

  let warpSettings = W.setPort (craneServerPort settings) . W.setHost (craneServerHost settings) $ W.defaultSettings

  W.runSettings warpSettings (waiAppForApp site app)

waiAppForApp :: CraneApp a => CraneSite -> a -> Application
waiAppForApp site app req respond =
    case lookupActionForRoute app req of
      Left (CouldNotMatchComponent _ _) -> notFound
      Left (UnableToInterpret _) -> notFound
      Left NoActionForRoute -> notFound
      Left Empty -> notFound
      Left NoHandlerForMethod -> respond (responseLBS badRequest400 [] "Invalid method")
      Right (action, AfterResponseActions afterResponse) ->
          do let craneRequest = CraneRequest site req app app id

             craneResponseBuilder <- runReaderT (runErrorT action) craneRequest
             case craneResponseBuilder of
               Right craneResponseBuilder ->
                   do let response = CraneResponse { crHeaders = []
                                                   , crStatusCode = status500
                                                   , crResponseBody = CraneResponseBodyBuilder mempty
                                                   , crSessions = M.empty }

                          response' = runResponseBuilder craneResponseBuilder craneRequest response

                      afterResponseResult <- runReaderT (runErrorT (afterResponse response')) craneRequest
                      case afterResponseResult of
                        Left      err  -> respond (responseLBS status500 [] (fromString ("Exception when running after response results: " ++ show err)))
                        Right (CraneResponse { crHeaders = headers
                                             , crStatusCode = status
                                             , crResponseBody = responseBody }) ->
                          respond (case responseBody of
                                     CraneResponseBodyBuilder builder -> responseBuilder status headers builder)
               Left err -> respond (responseLBS status500 [] (fromString (show err)))
    where notFound = respond (responseLBS notFound404 [] "Not found")