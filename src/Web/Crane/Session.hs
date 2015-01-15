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
import qualified Data.Text as T
import qualified Data.UUID as UUID

import System.Random

import Web.Cookie (SetCookie(..), renderSetCookie)

type SessionDict = M.Map T.Text JSON.Value

injectSessionIntoMiddleware :: CraneAddSession a => ((ByteString, IO (SessionFor a)) -> GenCraneMiddleware a master i o) -> GenCraneMiddleware a master i o
injectSessionIntoMiddleware f = f (cookieName (deduceAppProxy f), initialSession)
    where deduceAppProxy :: CraneAddSession a => ((ByteString, IO (SessionFor a)) -> GenCraneMiddleware a master i o) -> Proxy a
          deduceAppProxy _ = Proxy

session :: (CraneApp a, CraneAddSession a, ToJSON (SessionFor a), FromJSON (SessionFor a), ResultsIn b (CraneHandler a master)) => GenCraneMiddleware a master (SessionFor a -> b) b
session = injectSessionIntoMiddleware $ \(cookieName, initialSession) mkHandler ->
          do sessionId <- cookie cookieName

             tell (completeStoreSession cookieName sessionId)

                       -- sessionId <- case sessionId of -- Make a new session id, if we couldn't get one
                       --            Nothing -> UUID.toASCIIBytes <$> liftIO randomIO
                       --            Just sessId -> return sessId
             handler <- mkHandler
             return (monadifyArg handler $ do
                       sessBackend <- asks (csSessionBackend . crSite)
                       app <- asks crApp
                       case sessionId of
                         Nothing -> liftIO initialSession
                         Just sessionId ->
                             do contentOrError <- liftIO (runErrorT (csbFetchSession sessBackend sessionId))
                                case contentOrError of
                                  Right (Just sessionContents, storeSession) ->
                                    case JSON.decode (LBS.fromChunks [sessionContents]) :: Maybe SessionDict of
                                      Just sessionContents -> -- Lookup the key for this app in the session contents dictionary and try to parse it into the right format
                                          case M.lookup (sessionAppKey app) sessionContents of
                                            Nothing -> liftIO initialSession
                                            Just sessionContents ->
                                                case JSON.fromJSON sessionContents of
                                                  JSON.Error err -> throwError CorruptSessionContents
                                                  JSON.Success x -> return x
                                  Right (Nothing, storeSession) -> liftIO initialSession
                                  Left err -> throwError (CouldNotFetchSession err))

completeStoreSession :: CraneAddSession app => ByteString -> Maybe ByteString -> AfterResponseActions app master
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
                      sessBackend <- asks (csSessionBackend . crSite)
                      let storedSessions = crSessions response
                          ourSession = M.lookup (sessionAppKey app) storedSessions

                      case ourSession of
                        Nothing -> return response' -- If we have no new session to store, then just return the response to set the cookie
                        Just ourSessionContents ->
                            do contentOrError <- liftIO (runErrorT (csbFetchSession sessBackend sessionId))
                               case contentOrError of
                                 Left err -> throwError (CouldNotStoreSession err)
                                 Right (currentSessionContentsJSON, storeSession) ->
                                     do currentSessionContents <- case currentSessionContentsJSON of
                                                                    Just currentSessionContentsJSON ->
                                                                      case JSON.decode (LBS.fromChunks [currentSessionContentsJSON]) :: Maybe SessionDict of
                                                                        Just currentSessionContents -> return currentSessionContents
                                                                        Nothing -> throwError CorruptSessionContents
                                                                    Nothing -> return M.empty
                                        let newSessionContents' = M.insert (sessionAppKey app) ourSessionContents currentSessionContents
                                            newSessionContents'JSON = BS.concat . LBS.toChunks . JSON.encode $ newSessionContents'
                                        storeSessionResult <- liftIO (runErrorT (storeSession newSessionContents'JSON))
                                        case storeSessionResult of
                                          Left err -> throwError (CouldNotStoreSession err)
                                          Right () -> return response'

          deduceMasterProxy :: AfterResponseActions app master -> Proxy master
          deduceMasterProxy _ = Proxy

          masterProxy = deduceMasterProxy go

storeSession :: CraneAddSession a => SessionFor a -> CraneResponseBuilder a master
storeSession session = go
    where go = CraneResponseBuilder $ \req res ->
               res { crSessions = M.insert (sessionAppKey (crApp req)) (JSON.toJSON session) (crSessions res) }

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