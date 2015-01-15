{-# LANGUAGE OverloadedStrings, TypeFamilies, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, DefaultSignatures, TypeOperators, GADTs, StandaloneDeriving, DeriveDataTypeable #-}
module Web.Crane.Types where

import Blaze.ByteString.Builder

import Prelude hiding (id, (.))

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
import qualified Data.Aeson as JSON

import GHC.Generics

import Unsafe.Coerce

import Network.Wai as Wai
import Network.HTTP.Types

-- * CraneMonad

data CraneRequest app master = CraneRequest
                             { crSite :: CraneSite
                             , crWaiRequest :: Request
                             , crApp :: app
                             , crMaster :: master
                             , crAppRouteToMaster :: RoutesFor app -> RoutesFor master}
data CraneResponse = CraneResponse
                     { crHeaders :: ResponseHeaders
                     , crStatusCode :: Status
                     , crResponseBody :: CraneResponseBody
                     , crSessions :: M.Map Text JSON.Value }
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
newtype PathParser pc app master i o = PathParser { runPathParser :: pc app master i o }

emptyResponse = CraneResponse [] ok200 (CraneResponseBodyBuilder mempty)

getApp :: CraneMonad app master app
getApp = getApp' id
getMaster :: CraneMonad app master master
getMaster = getMaster' id

getApp' :: (app -> a) -> CraneMonad app master a
getApp' f = asks (f . crApp)
getMaster' :: (master -> a) -> CraneMonad app master a
getMaster' f = asks (f . crMaster)

-- * Routes

data RouteParseState app master = RouteParseState
                                { pathComponents :: [Text]
                                , rpsWaiRequest :: Wai.Request
                                , rpsParsedCookies :: [(ByteString, ByteString)]
                                , rpsApp :: app
                                , rpsMaster :: master }

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

class ToPathComponent a where
    toPathComponent :: a -> Text

class FromPathComponent a where
    fromPathComponent :: Text -> Maybe a

newtype ConstructHandler app master a = ConstructHandler { runConstructHandler :: WriterT (AfterResponseActions app master) (ErrorT RouteParseError (Reader (RouteParseState app master, RoutesFor app))) a }
    deriving (Monad, Applicative, Functor, MonadError RouteParseError, MonadReader (RouteParseState app master, RoutesFor app), MonadWriter (AfterResponseActions app master))
newtype ActionDispatch app master a = ActionDispatch { runActionDispatch :: ErrorT RouteParseError (Reader (RouteParseState app master)) a }
    deriving (Monad, Applicative, Functor, MonadError RouteParseError, MonadReader (RouteParseState app master))
newtype RouteDispatch app master i a = RouteDispatch { runRouteDispatch :: (i, RouteParseState app master) -> Either RouteParseError (a, RouteParseState app master) }

data GenericPathComponent where
    GenericPathComponent :: (Typeable a, ToPathComponent a) => a -> GenericPathComponent

class PathContext pc where
    popPathComponent :: (Arrow (pc app master), ArrowChoice (pc app master)) => Either Text (GenericPathComponent -> Text) -> pc app master i (Maybe Text)
    routeParseError  :: (Arrow (pc app master), ArrowChoice (pc app master)) => pc app master RouteParseError b

class PathContext pc => ActionContext ac pc | ac -> pc where
    ($|) :: ac app master (CraneHandler app master, AfterResponseActions app master) ->
            ac app master (CraneHandler app master, AfterResponseActions app master) ->
            ac app master (CraneHandler app master, AfterResponseActions app master)

    ($-) :: ac app master (RoutesFor app) ->
            ConstructHandler app master (CraneHandler app master) ->
            ac app master (CraneHandler app master, AfterResponseActions app master)

    path :: ( CraneApp app, CraneApp master
            , ResultsInRoute a (RoutesFor app)) =>
            a ->
            PathParser pc app master a (RoutesFor app) ->
            ac app master (RoutesFor app)

    mount :: ( ResultsInRoute a (RoutesFor app)
             , IsCraneSubsite app sub, CraneApp master) =>
             a ->
             PathParser pc app master a (RoutesFor sub -> RoutesFor app) ->
             ac app master (CraneHandler app master, AfterResponseActions app master)

class (CraneApp app, CraneApp sub) => IsCraneSubsite app sub where
    -- | Subsites can be instantiated multiple times under different routes. The first parameter lets us select
    --   the subsite based on the route under consideration. The function should not be strict in the route
    --   for the subsite, since that will be undefined. The other constructor arguments will be valid
    subsiteFromMaster :: RoutesFor app -> app -> sub

infixl 3 $|
infixr 4 $-

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

instance (DeconstructRoute (f x), DeconstructRoute (g x)) => DeconstructRoute ((:*:) f g x) where
    deconstructRoute (f :*: g) = deconstructRoute f ++ deconstructRoute g

instance (ToPathComponent c, Typeable c) => DeconstructRoute (K1 R c x) where
    deconstructRoute (K1 c) = [GenericPathComponent c]

-- | `RoutesFor a` components are handled in a special way. These only arise when we're using subsites (there's no FromPathComponent instance, so they can't be used with Var)
instance CraneApp a => ToPathComponent (RoutesFor a) where
    toPathComponent _ = undefined

-- * Apps

class ( ResultsInRoute (RoutesFor a) (RoutesFor a)
      , ConEq (RoutesFor a)
      , DeconstructRoute (RoutesFor a)
      , Typeable a) => CraneApp a where
    data RoutesFor a  :: *

    routes :: ( CraneApp master
              , ActionContext ac pc, Arrow (pc a master), ArrowChoice (pc a master)) => a -> ac a master (CraneHandler a master, AfterResponseActions a master)

deriving instance Typeable RoutesFor

class ( CraneApp a
      , FromJSON (SessionFor a)
      , ToJSON (SessionFor a)) => CraneAddSession a where
    data SessionFor a :: *

    initialSession   :: IO (SessionFor a)

    cookieName       :: Proxy a -> ByteString
    cookieName      _ = "SESSIONID"

    sessionAppKey    :: a -> Text

    default sessionAppKey :: Show a => a -> Text
    sessionAppKey = fromString . show

instance CraneApp a  => ResultsInRoute (RoutesFor a) (RoutesFor a)
instance (CraneApp a, Generic (RoutesFor a), ConEq (Rep (RoutesFor a) ())) => ConEq (RoutesFor a)
instance (CraneApp a, Generic (RoutesFor a), DeconstructRoute (Rep (RoutesFor a) ())) => DeconstructRoute (RoutesFor a)
instance (CraneAddSession a, Generic (SessionFor a),
          GFromJSON (Rep (SessionFor a))) => FromJSON (SessionFor a)
instance (CraneAddSession a, Generic (SessionFor a),
          GToJSON (Rep (SessionFor a))) => ToJSON (SessionFor a)
-- instance CraneApp a master => IsCraneSubsite a a master where -- All sites are subsites of themselves
--     subsiteFromMaster _ a = a

-- ** Middleware

type GenCraneMiddleware app master i o = ConstructHandler app master i -> ConstructHandler app master o

(<:>) :: CraneApp a => ConstructHandler a master i -> GenCraneMiddleware a master i o -> ConstructHandler a master o
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

-- * Sites

data CraneSiteSpec a = CraneSiteSpec
                     { mkSessionBackend :: IO CraneSessionBackend
                     , rootApplication  :: a }

data CraneSite = CraneSite
               { csSessionBackend :: CraneSessionBackend }