{-# LANGUAGE OverloadedStrings #-}
module Web.Crane.Wai where

import Web.Crane.Types
import Web.Crane.Routes

import Control.Monad.Reader
import Control.Monad.Error

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import Data.String
import Data.Monoid

import Network.Wai
import Network.HTTP.Types.Status

craneWaiApp :: CraneApp a => a -> Application
craneWaiApp app req respond =
    case lookupActionForRoute app req of
      Left (CouldNotMatchComponent _ _) -> notFound
      Left (UnableToInterpret _) -> notFound
      Left NoActionForRoute -> notFound
      Left Empty -> notFound
      Left NoHandlerForMethod -> respond (responseLBS badRequest400 [] "Invalid method")
      Right (action, AfterResponseActions afterResponse) ->
          do let craneRequest = CraneRequest req app

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