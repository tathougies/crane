{-# LANGUAGE FlexibleContexts, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, TypeFamilies, OverloadedStrings #-}
module Web.Crane.Session
    ( session
    , storeSession

    , mkMemorySessionBackend ) where

import Web.Crane.Types
import Web.Crane.Request (cookie)
import Web.Crane.Response (header, modifyResponse)
import Web.Crane.Internal

import Blaze.ByteString.Builder

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Concurrent.STM

import Data.Aeson as JSON
import Data.Typeable
import Data.Proxy
import Data.Default
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import qualified Data.UUID as UUID

import System.Random

import Web.Cookie (SetCookie(..), renderSetCookie)

injectSessionIntoMiddleware :: CraneAddSession a => ((ByteString, IO (SessionFor a)) -> GenCraneMiddleware a i o) -> GenCraneMiddleware a i o
injectSessionIntoMiddleware f = f (cookieName (deduceProxy f), initialSession)
    where deduceProxy :: CraneAddSession a => ((ByteString, IO (SessionFor a)) -> GenCraneMiddleware a i o) -> Proxy a
          deduceProxy _ = Proxy

session :: (CraneApp a, CraneAddSession a, ToJSON (SessionFor a), FromJSON (SessionFor a), ResultsIn b (CraneHandler a)) => GenCraneMiddleware a (SessionFor a -> b) b
session = injectSessionIntoMiddleware $ \(cookieName, initialSession) mkHandler ->
          do sessBackend <- asks (sessionBackend . rpsApp . fst)
             sessionId <- cookie cookieName

             tell (completeStoreSession cookieName sessionId)

                       -- sessionId <- case sessionId of -- Make a new session id, if we couldn't get one
                       --            Nothing -> UUID.toASCIIBytes <$> liftIO randomIO
                       --            Just sessId -> return sessId
             handler <- mkHandler
             return (monadifyArg handler $ do
                       case sessionId of
                         Nothing -> liftIO initialSession
                         Just sessionId ->
                             do contentOrError <- liftIO (runErrorT (csbFetchSession sessBackend sessionId))
                                case contentOrError of
                                  Right (Just sessionContents, storeSession) ->
                                    case JSON.decode (LBS.fromChunks [sessionContents]) of
                                      Just sessionContents -> return sessionContents
                                      Nothing -> throwError CorruptSessionContents
                                  Right (Nothing, storeSession) -> liftIO initialSession
                                  Left err -> throwError (CouldNotFetchSession err))

completeStoreSession :: CraneAddSession app => ByteString -> Maybe ByteString -> AfterResponseActions app
completeStoreSession cookieName sessionId = go
    where go = AfterResponseActions $
               \response ->
                   do sessionId <- case sessionId of
                                     Just sessionId -> return sessionId
                                     Nothing        -> UUID.toASCIIBytes <$> liftIO randomIO

                      let setCookie = def { setCookieName = cookieName
                                          , setCookieValue = sessionId }
                          setCookieHeader = toByteString . renderSetCookie $ setCookie

                      -- Generate response with correct Set-Cookie header
                      response' <- runAfterResponseActions (modifyResponse (header "Set-Cookie" setCookieHeader)) response

                      -- Now store the session in the backend...
                      app <- asks crApp
                      sessBackend <- asks (sessionBackend . crApp)
                      let storedSessions = crSessions response
                          ourSession = M.lookup (typeOf app) storedSessions

                      case ourSession of
                        Nothing -> return response' -- If we have no new session to store, then just return the response to set the cookie
                        Just ourSessionContents ->
                            do contentOrError <- liftIO (runErrorT (csbFetchSession sessBackend sessionId))
                               case contentOrError of
                                 Left err -> throwError (CouldNotStoreSession err)
                                 Right (_, storeSession) ->
                                     do storeSessionResult <- liftIO (runErrorT (storeSession ourSessionContents))
                                        case storeSessionResult of
                                          Left err -> throwError (CouldNotStoreSession err)
                                          Right () -> return response'

storeSession :: CraneAddSession a => SessionFor a -> CraneResponseBuilder a
storeSession session = go
    where go = CraneResponseBuilder $ \req res ->
               res { crSessions = M.insert appTypeRep (BS.concat . LBS.toChunks . JSON.encode $ session) (crSessions res) }

          -- Haskell magic...
          appProxy :: CraneResponseBuilder a -> a
          appProxy _ = undefined

          appTypeRep = typeOf (appProxy go)

-- * Session backends

-- | Simple backend for sessions that stores all sessions in a large Haskell map. This backend does not take into account session expiry,
--   and so should only be used for testing
mkMemorySessionBackend :: IO CraneSessionBackend
mkMemorySessionBackend = do
  sessionStore <- newTVarIO M.empty
  return CraneSessionBackend
         { csbFetchSession = \key ->
                             do value <- liftIO (atomically (M.lookup key <$> readTVar sessionStore))
                                return (value, \newValue -> liftIO $ atomically $ modifyTVar sessionStore (M.insert key newValue)) }