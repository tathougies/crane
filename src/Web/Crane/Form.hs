{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}
module Web.Crane.Form where

import Web.Crane.Types
import Web.Crane.Internal
import Web.Crane.Request

import Control.Arrow
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Error

import Data.List (lookup)
import Data.Char
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M

import Network.Wai
import Network.HTTP.Types

import Text.Digestive

-- -- TODO forms submitted with get?
-- runFormGET :: Text                 -- ^ Name for the form
--            -> Form v CraneMonad a  -- ^ Form to run
--            -> CraneMonad View v    -- ^ Result
-- runFormGET name form = getForm name form

-- runFormPOST :: Text                         -- ^ Name for the form
--             -> Form v CraneMonad a          -- ^ Form to run
--             -> CraneMonad (View v, Maybe a) -- ^ Result
-- runFormPOST name form = postForm name form craneEnv

-- craneEnv :: FormEncType ->

multipartFormType, urlEncodedFormType :: BS.ByteString
multipartFormType = "multipart/form-data"
urlEncodedFormType = "application/x-www-form-urlencoded"

form :: (CraneApp app, ResultsIn o (CraneHandler app master)) => Text -> Form v (CraneMonad app master) a -> GenCraneMiddleware app master ((View v, Maybe a) -> o) o
form formName digestiveForm mkHandler =
     do -- Depending on the method type, we should check that all required keys/etc do indeed exist
        -- This will ignore any multipart encoded forms (since they may contain file uploads). If you want to use forms with file uploads,
        -- please use the formFileUpload function

        encType <- reqHeader contentTypeHeaderName
        request <- asks (rpsWaiRequest . fst)
        queryString <- case requestMethod request of
                         x | x == methodGet  -> return (Left (rawQueryString request))
                           | x == methodPost ->
                               case encType of
                                 Nothing -> throwError (MissingHeader contentTypeHeaderName)
                                 Just x
                                     | x == multipartFormType -> throwError UnexpectedFileUpload
                                     | x == urlEncodedFormType -> return (Right (strictRequestBody request))
                                     | otherwise -> throwError (UnsupportedContentType x)

        -- If we weren't supplied a query string and the form was submitted using GET, then we assume that we're not getting any data
        -- from this form. Thus, we just render the form and are done with it
        handler <- mkHandler
        return (monadifyArg handler $
                do queryString <- case queryString of
                                    Left simpleString -> return simpleString
                                    Right     readAll -> BS.concat . LBS.toChunks <$> liftIO readAll

                   -- This parses a query string into a M.Map Text Text
                   let formData = let keyValuePairs = BS.split (fromIntegral $ ord '&') queryString
                                      keysAndValues = map (second BS.tail) . map (BS.break (== (fromIntegral $ ord '='))) $ keyValuePairs
                                  in map ((urlDecode True *** urlDecode True) >>> (TE.decodeUtf8 *** TE.decodeUtf8)) keysAndValues

                       -- Construct the digestive-functors environment
                       environment path = let inputName = fromPath path
                                              matchingFieldValues = map snd . filter (fst >>> (==inputName)) $ formData
                                          in return (map TextInput matchingFieldValues)

                       getEnvironment UrlEncoded = return environment
                       getEnvironment MultiPart  = throwError (LiftedRPError (WrongFormEncodingType multipartFormType urlEncodedFormType))

                   if requestMethod request == methodGet && BS.null queryString
                   then (,Nothing) <$> getForm formName digestiveForm
                   else postForm formName digestiveForm getEnvironment)