{-# LANGUAGE OverloadedStrings, TypeFamilies, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, DefaultSignatures, TypeOperators, GADTs #-}
module Web.Crane.Types where

import Blaze.ByteString.Builder

import Prelude hiding (id)

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.Identity
import qualified Control.Exception as E

import Data.String
import Data.Proxy
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Data.ByteString
import Data.Typeable
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as M

import GHC.Generics

import Network.Wai as Wai
import Network.HTTP.Types

-- * CraneMonad

data CraneRequest app master = CraneRequest
                             { crWaiRequest :: Request
                             , crApp :: app
                             , crAppRouteToMaster :: RoutesFor app -> RoutesFor master}
data CraneResponse = CraneResponse
                     { crHeaders :: ResponseHeaders
                     , crStatusCode :: Status
                     , crResponseBody :: CraneResponseBody
                     , crSessions :: M.Map TypeRep ByteString }
data CraneResponseBody = CraneResponseBodyBuilder Builder
newtype CraneResponseBuilder app master = CraneResponseBuilder { runResponseBuilder :: CraneRequest app master -> CraneResponse -> CraneResponse }

data CraneHandlerError = CorruptSessionContents
                       | CouldNotStoreSession SessionBackendError
                       | CouldNotFetchSession SessionBackendError
                       | LiftedRPError RouteParseError
                       | CustomHandlerError String
                       deriving (Show, Eq, Ord)
instance Error CraneHandlerError

type CraneHandler app master = CraneMonad app master (CraneResponseBuilder app master)
type CraneMonad app master = ErrorT CraneHandlerError (ReaderT (CraneRequest app master) IO)

emptyResponse = CraneResponse [] ok200 (CraneResponseBodyBuilder mempty)

-- * Routes

data RouteParseState app master = RouteParseState
                                { pathComponents :: [Text]
                                , rpsWaiRequest :: Wai.Request
                                , rpsParsedCookies :: [(ByteString, ByteString)]
                                , rpsApp :: app
                                , rpsAppRouteToMaster :: RoutesFor app -> RoutesFor master }

newtype AfterResponseActions app master = AfterResponseActions { runAfterResponseActions :: CraneResponse -> CraneMonad app master CraneResponse }

instance Monoid (AfterResponseActions app master) where
    mempty = AfterResponseActions return
    mappend (AfterResponseActions a) (AfterResponseActions b) = AfterResponseActions $ \res -> a res >>= b

data RouteParseError = CouldNotMatchComponent Text Text
                     | UnableToInterpret String
                     | MissingHeader (CI.CI ByteString)
                     | UnsupportedContentType ByteString
                     | UnexpectedFileUpload
                       -- | `WrongFormEncodingType expecting actual`
                     | WrongFormEncodingType ByteString ByteString
                     | NoActionForRoute
                     | NoHandlerForMethod
                     | Empty
                       deriving (Show, Read, Eq, Ord)
instance Error RouteParseError

data Var a = Var
           deriving (Show)

class ToPathComponent a where
    toPathComponent :: a -> Text

class FromPathComponent a where
    fromPathComponent :: Text -> Maybe a

newtype ConstructHandler app master a = ConstructHandler { runConstructHandler :: WriterT (AfterResponseActions app master) (ErrorT RouteParseError (Reader (RouteParseState app master, RoutesFor app master))) a }
    deriving (Monad, Applicative, Functor, MonadError RouteParseError, MonadReader (RouteParseState app, RoutesFor app), MonadWriter (AfterResponseActions app))
newtype ActionDispatch app master a = ActionDispatch { runActionDispatch :: ErrorT RouteParseError (Reader (RouteParseState app)) a }
    deriving (Monad, Applicative, Functor, MonadError RouteParseError, MonadReader (RouteParseState app))
newtype RouteDispatch app master i a = RouteDispatch { runRouteDispatch :: (i, RouteParseState app) -> Either RouteParseError (a, RouteParseState app) }

data GenericPathComponent where
    GenericPathComponent :: ToPathComponent a => a -> GenericPathComponent

class PathContext pc where
    popPathComponent :: (Arrow (pc app), ArrowChoice (pc app)) => Either Text (GenericPathComponent -> Text) -> pc app i (Maybe Text)
    routeParseError  :: (Arrow (pc app), ArrowChoice (pc app)) => pc app RouteParseError b

class PathComponent c input output | c -> input, c input -> output where
    (*/) :: (PathContext m, Arrow (m app), ArrowChoice (m app)) => m app i input -> c -> m app i output

class PathContext pc => ActionContext ac pc | ac -> pc where
    ($|) :: ac app (CraneHandler app, AfterResponseActions app) -> ac app (CraneHandler app, AfterResponseActions app) -> ac app (CraneHandler app, AfterResponseActions app)
    ($-) :: ac app (RoutesFor app) -> ConstructHandler app (CraneHandler app) -> ac app (CraneHandler app, AfterResponseActions app)
    path :: (ConEq (RoutesFor app), DeconstructRoute (RoutesFor app), ResultsInRoute a (RoutesFor app)) => a -> pc app a (RoutesFor app) -> ac app (RoutesFor app)
    mount :: (ConEq (RoutesFor app), DeconstructRoute (RoutesFor app), ResultsInRoute a (RoutesFor app), IsCraneSubsite app sub) => a -> pc app a (RoutesFor sub -> RoutesFor app)  -> ac app (CraneHandler app, AfterResponseActions app)

class (CraneApp master, CraneApp sub) => IsCraneSubsite master sub where
    -- | Subsites can be instantiated multiple times under different routes. The first parameter lets us select
    --   the subsite based on the route under consideration. The function should not be strict in the route
    --   for the subsite, since that will be undefined. The other constructor arguments will be valid
    subsiteFromMaster :: RoutesFor master -> master -> sub

infixl 3 $|
infixr 4 $-

root :: (PathContext pc, Category (pc app)) => pc app a a
root = id

class ResultsInRoute a r | a -> r where
    getRoute :: a -> r

    default getRoute :: a ~ r => a -> r
    getRoute = id

instance ResultsInRoute b r => ResultsInRoute (a -> b) r where
    getRoute f = getRoute (f undefined)

-- ** URI reversing a la Django

class ConEq a where
    -- | Determines if the two inputs were created with the same constructor. Should be non-strict in each of the constructor arguments
    (@==) :: a -> a -> Bool

    default (@==) :: (Generic a, ConEq (Rep a ())) => a -> a -> Bool
    a @== b = from' a @== from' b
        where from' :: Generic a => a -> Rep a ()
              from' = from

-- In order to test a datatype for equality, we have to test which constructor it's using
instance ConEq (p x) => ConEq (M1 D f p x) where
    M1 a @== M1 b = a @== b

-- If we're looking at two constructor
instance ConEq (M1 C f p x) where
    M1 a @== M1 b = True

-- When looking at sum types, only when both constructor choices are the same can we continue
instance (ConEq (f x), ConEq (g x)) => ConEq ((f :+: g) x) where
    L1 a @== L1 b = a @== b
    R1 a @== R1 b = a @== b
    _ @== _ = False

class DeconstructRoute a where
    deconstructRoute :: a -> [GenericPathComponent]

    default deconstructRoute :: (Generic a, DeconstructRoute (Rep a ())) => a -> [GenericPathComponent]
    deconstructRoute a = deconstructRoute (from' a)
        where from' :: Generic a => a -> Rep a ()
              from' = from

instance DeconstructRoute (p x) => DeconstructRoute (M1 D f p x) where
    deconstructRoute (M1 a) = deconstructRoute a

instance (DeconstructRoute (f x), DeconstructRoute (g x)) => DeconstructRoute ((:+:) f g x) where
    deconstructRoute (L1 a) = deconstructRoute a
    deconstructRoute (R1 a) = deconstructRoute a

instance DeconstructRoute (p x) => DeconstructRoute (M1 C f p x) where
    deconstructRoute (M1 a) = deconstructRoute a

instance DeconstructRoute (U1 x) where
    deconstructRoute U1 = []

instance DeconstructRoute (p x) => DeconstructRoute (M1 S f p x) where
    deconstructRoute (M1 a) = deconstructRoute a

instance ToPathComponent c => DeconstructRoute (K1 R c x) where
    deconstructRoute (K1 c) = [GenericPathComponent c]

instance (DeconstructRoute (f x), DeconstructRoute (g x)) => DeconstructRoute ((:*:) f g x) where
    deconstructRoute (f :*: g) = deconstructRoute f ++ deconstructRoute g

-- * Apps

class ( ResultsInRoute (RoutesFor a) (RoutesFor a)
      , ConEq (RoutesFor a)
      , DeconstructRoute (RoutesFor a)
      , Typeable a) => CraneApp a where
    data RoutesFor a  :: *

    routes :: (ActionContext ac pc, Arrow (pc a), ArrowChoice (pc a)) => a -> ac a (CraneHandler a, AfterResponseActions a)

class ( CraneApp a
      , FromJSON (SessionFor a)
      , ToJSON (SessionFor a)) => CraneAddSession a where
    data SessionFor a :: *

    initialSession   :: IO (SessionFor a)
    sessionBackend :: a -> CraneSessionBackend
    cookieName       :: Proxy a -> ByteString
    cookieName      _ = "SESSIONID"

instance CraneApp a => ResultsInRoute (RoutesFor a) (RoutesFor a)
instance (CraneApp a, Generic (RoutesFor a), ConEq (Rep (RoutesFor a) ())) => ConEq (RoutesFor a)
instance (CraneApp a, Generic (RoutesFor a), DeconstructRoute (Rep (RoutesFor a) ())) => DeconstructRoute (RoutesFor a)
instance (CraneAddSession a, Generic (SessionFor a),
          GFromJSON (Rep (SessionFor a))) => FromJSON (SessionFor a)
instance (CraneAddSession a, Generic (SessionFor a),
          GToJSON (Rep (SessionFor a))) => ToJSON (SessionFor a)

-- ** Middleware

type GenCraneMiddleware app i o = ConstructHandler app i -> ConstructHandler app o

(<:>) :: CraneApp a => ConstructHandler a i -> GenCraneMiddleware a i o -> ConstructHandler a o
handler <:> middleware = middleware handler

infixl 4 <:>

-- ** Sessions

type StoreSession = ByteString -> ErrorT SessionBackendError IO ()

data SessionBackendError = CouldNotCreateBackend String
                         | CustomSessionError String
                           deriving (Show, Eq, Ord)
instance Error SessionBackendError

data CraneSessionBackend = CraneSessionBackend
                         { csbFetchSession :: ByteString -> ErrorT SessionBackendError IO (Maybe ByteString, StoreSession) }

-- * Useful constants

cookieHeaderName :: CI.CI ByteString
cookieHeaderName = "Cookie"

contentTypeHeaderName :: CI.CI ByteString
contentTypeHeaderName = "Content-type"