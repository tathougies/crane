{-# LANGUAGE OverloadedStrings #-}
module Web.Crane.Response where

import Web.Crane.Types

import Blaze.ByteString.Builder

import Control.Monad.Reader

import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import Data.Monoid
import Data.ByteString (ByteString)

import Network.HTTP.Types

import Text.Blaze
import Text.Blaze.Renderer.Utf8 (renderMarkup)

instance Monoid (CraneResponseBuilder app) where
    mempty = CraneResponseBuilder (\_ res -> res)
    mappend (CraneResponseBuilder a) (CraneResponseBuilder b) =
        CraneResponseBuilder (\s -> b s . a s)

status :: Status -> CraneResponseBuilder app
status statusCode = CraneResponseBuilder (\_ res -> res { crStatusCode = statusCode })

responseBodyLBS :: LBS.ByteString -> CraneResponseBuilder app
responseBodyLBS body = CraneResponseBuilder (\_ res -> res { crResponseBody = CraneResponseBodyBuilder . fromLazyByteString $ body })

htmlResponseBody :: LBS.ByteString -> CraneResponseBuilder app
htmlResponseBody body = header "Content-type" "text/html" <>
                        responseBodyLBS body

htmlMarkupBody :: Markup -> CraneResponseBuilder app
htmlMarkupBody markup = htmlResponseBody (renderMarkup markup)

header :: ByteString -> ByteString -> CraneResponseBuilder app
header name value = CraneResponseBuilder (\_ res -> res { crHeaders = (CI.mk name, value):crHeaders res })

redirect :: Text -> CraneResponseBuilder app
redirect uri = status seeOther303 <>
               header "Location" (TE.encodeUtf8 uri)


-- | This lets AfterResponseActions (usually generated by middlewares) modify the response before it is sent out
modifyResponse :: CraneResponseBuilder app -> AfterResponseActions app
modifyResponse modifier  = AfterResponseActions $
                           \response -> do request <- ask
                                           return (runResponseBuilder modifier request response)