{-# LANGUAGE DoAndIfThenElse #-}
module Web.Crane.Request where

import Web.Crane.Types
import Web.Crane.Routes

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Identity

import Data.ByteString
import qualified Data.CaseInsensitive as CI

import Network.Wai
import Network.HTTP.Types

checkMethod :: Method -> ConstructHandler app (RoutesFor app -> CraneHandler app) -> ConstructHandler app (CraneHandler app)
checkMethod expMethod routeAction =
    do method <- asks (requestMethod . rpsWaiRequest . fst)

       if method == expMethod
       then routeAction <*> asks snd
       else throwError NoHandlerForMethod

get = checkMethod methodGet
post = checkMethod methodPost

-- * Cookies

cookie :: ByteString -> ConstructHandler app (Maybe ByteString)
cookie cookieName = do cookies <- asks (rpsParsedCookies . fst)
                       return (lookup cookieName cookies)

reqHeader :: CI.CI ByteString -> ConstructHandler app (Maybe ByteString)
reqHeader name = do headers <- asks (requestHeaders . rpsWaiRequest . fst)
                    return (lookup name headers)