{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, GeneralizedNewtypeDeriving, RankNTypes, DoAndIfThenElse, GADTs, KindSignatures, FlexibleContexts, TupleSections #-}
module Web.Crane.Routes where

import Web.Crane.Types
import Web.Crane.Internal

import Prelude hiding (id, (.))

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.Writer
import Control.Category
import Control.Arrow

import Data.String
import Data.Maybe
import Data.Proxy
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T

import Network.Wai

import Unsafe.Coerce

import Web.Cookie (parseCookies)

-- * Path Components

data ReverseState app = ReverseState
                    { rsRoute :: (RoutesFor app)
                    , rsDone  :: ExitMonadFunction (ReverseContextM app) (Maybe T.Text) }

type ReversePathState = [GenericPathComponent]

data ExitMonadFunction m a where
    ExitMonadFunction :: (a -> m r) -> ExitMonadFunction m a

type ReverseContextM app = ReaderT (ReverseState app) (Cont (Maybe T.Text))

newtype ReverseContext app a = ReverseContext { runReverseContext :: ReverseContextM app a }
    deriving (MonadReader (ReverseState app), MonadCont, Monad, Applicative, Functor)

newtype ReversePathArrow app i a = ReversePathArrow { runReversePath :: (T.Text, ReversePathState) -> (T.Text, ReversePathState) }

instance ToPathComponent GenericPathComponent where
    toPathComponent (GenericPathComponent x) = toPathComponent x

instance PathContext RouteDispatch where
    popPathComponent _ = RouteDispatch $ \(_, state) -> case pathComponents state of
                                                          [] -> Right (Nothing, state)
                                                          x:xs -> Right (Just x, state { pathComponents = xs} )
    routeParseError = RouteDispatch (Left . fst)

instance ActionContext ActionDispatch RouteDispatch where
    ActionDispatch a $| ActionDispatch b = ActionDispatch (catchError a (const b))
    ActionDispatch a $- ConstructHandler b = ActionDispatch $
                                           do route <- a
                                              mapErrorT (withReader (, route)) (runWriterT b)

    path pathConstructor parsePath = ActionDispatch $
                                     do state <- ask
                                        case runRouteDispatch parsePath (pathConstructor, state) of
                                          Left err -> throwError err
                                          Right (x, state') -> case pathComponents state' of
                                                                 []   -> return x
                                                                 [""] -> return x
                                                                 _    -> throwError NoActionForRoute
    mount pathConstructor parsePath = ActionDispatch $
                                      do state <- ask
                                         case runRouteDispatch parsePath (pathConstructor, state) of
                                           Left err -> throwError err

                                           -- If this succeeds, then we need to parse the remaining components using the subsite's route parser
                                           Right (x, state') ->
                                               do let subsite = subsiteFromMaster' x masterRoute (rpsApp state)
                                                      subsiteRps = state' { rpsApp = subsite
                                                                          , pathComponents = pathComponents state' }

                                                      dispatch = routes subsite
                                                      masterRoute = x undefined -- This is used as input to subsiteFromMaster to get the right subsite instance

                                                      subsiteParseResult = runReader (runErrorT (runActionDispatch dispatch)) subsiteRps

                                                      -- Force the type system into getting the right subsite type
                                                      subsiteFromMaster' :: IsCraneSubsite app sub => (RoutesFor sub -> RoutesFor app) -> RoutesFor app -> app -> sub
                                                      subsiteFromMaster' _ = subsiteFromMaster

                                                  case subsiteParseResult of
                                                    Left err -> throwError err
                                                    Right (subHandler, subAfterResponseActions) ->
                                                        let masterHandler = mapCraneHandler (subsiteFromMaster masterRoute) subHandler
                                                            masterAfterResponseActions = mapAfterResponseActions (subsiteFromMaster masterRoute) subAfterResponseActions
                                                        in return (masterHandler, masterAfterResponseActions)

(*-) :: ConstructHandler app (CraneHandler app) -> ConstructHandler app (CraneHandler app) -> ConstructHandler app (CraneHandler app)
ConstructHandler a *- ConstructHandler b = ConstructHandler (catchError a (const b))
infixl 5 *-

instance Category (RouteDispatch app) where
    id = RouteDispatch Right
    RouteDispatch a . RouteDispatch b = RouteDispatch $ \(i, state) -> case b (i, state) of
                                                                         Left err -> Left err
                                                                         Right (i', state') -> a (i', state')

instance Arrow (RouteDispatch app) where
    arr f = RouteDispatch $ \(i, state) -> Right (f i, state)
    first (RouteDispatch f) = RouteDispatch $ \((b, d), state) -> case f (b, state) of
                                                                    Left err -> Left err
                                                                    Right (b', state') -> Right ((b', d), state')

instance ArrowChoice (RouteDispatch app) where
    left (RouteDispatch f) =
        RouteDispatch $ \(i, state) ->
            case i of
              Left x  -> case f (x, state) of
                           Left err -> Left err
                           Right (x', state') -> Right (Left x', state')
              Right x -> Right (Right x, state)

instance PathComponent T.Text a a where
    a */ x = a >>>
             (popPathComponent (Left x) &&& id) >>>
             arr (\(component, input) ->
                      case component of
                        Just c  -> if c == x then Right input else Left (CouldNotMatchComponent x c)
                        Nothing -> Left (CouldNotMatchComponent x "")) >>>
             (routeParseError ||| id)

instance (FromPathComponent v, ToPathComponent v) => PathComponent (Var v) (v -> b) b where
    a */ Var  = a >>> (parseComponent &&& id) >>> -- We need to do a >>> (parseComponent &&& id) so that a gets to parse first. Otherwise, parseComponent and a both get the same thing
                arr (\(parsedArg, f) -> f parsedArg)
        where parseComponent = popPathComponent (Right toPathComponent) >>>
                               arr (\c -> case fromPathComponent =<< c of
                                            Just x -> Right x
                                            _      -> Left (UnableToInterpret (show c))) >>>
                               (routeParseError ||| id)

lookupActionForRoute :: CraneApp app => app -> Request -> Either RouteParseError (CraneHandler app, AfterResponseActions app)
lookupActionForRoute app req =
    let routeParseState = RouteParseState { pathComponents = pathInfo req
                                          , rpsWaiRequest = req
                                          , rpsParsedCookies = case cookieHeader of
                                                                 Just cookieHeader -> parseCookies cookieHeader
                                                                 Nothing -> []
                                          , rpsApp = app }

        cookieHeader = lookup cookieHeaderName (requestHeaders req)

        dispatch = routes app
    in runReader (runErrorT (runActionDispatch dispatch)) routeParseState

-- * URI Reversal

finish :: Maybe T.Text -> ReverseContextM app a
finish result = do st <- ask
                   case st of
                     -- XXX unsafeCoerce...
                     ReverseState { rsDone = ExitMonadFunction finish' } -> (unsafeCoerce finish') result

instance Category (ReversePathArrow app) where
    id = ReversePathArrow id
    ReversePathArrow a . ReversePathArrow b = ReversePathArrow (a . b)

instance Arrow (ReversePathArrow app) where
    arr f = ReversePathArrow id
    first (ReversePathArrow f) = ReversePathArrow f

instance ArrowChoice (ReversePathArrow app) where
    left (ReversePathArrow f) = ReversePathArrow f

instance PathContext ReversePathArrow where
    popPathComponent f = ReversePathArrow $ \(pre, components) ->
                         case f of
                           Left x -> (T.concat [pre, "/", x], components)
                           Right f -> case components of
                                        x:xs -> (T.concat [pre, "/", f x], xs)
                                        []   -> error "Out of components"
    routeParseError = ReversePathArrow id

instance ActionContext ReverseContext ReversePathArrow where
    ReverseContext a $| ReverseContext b = ReverseContext $
                                           do aRes <- callCC (\finish' -> withReaderT (\rs -> rs { rsDone = ExitMonadFunction finish' }) (a >> return Nothing))
                                              case aRes :: Maybe T.Text of
                                                Nothing -> do bRes <- callCC (\finish' -> withReaderT (\rs -> rs { rsDone = ExitMonadFunction finish' }) (b >> return Nothing))
                                                              case bRes of
                                                                Nothing -> finish Nothing
                                                                _       -> finish bRes
                                                _       -> finish aRes
    ReverseContext a $- _ = ReverseContext $ a >> finish Nothing
    path routeCon parseRoute = ReverseContext $
                               do let route = getRoute routeCon
                                  expRoute <- asks rsRoute
                                  if route @== expRoute
                                  then finish (Just (fst (runReversePath parseRoute ("", deconstructRoute expRoute))))
                                  else finish Nothing

reversedURI :: (ConEq (RoutesFor app), DeconstructRoute (RoutesFor app)) => ReverseContext app a -> RoutesFor app -> Maybe T.Text
reversedURI routes r = runCont (runReaderT go (ReverseState r undefined)) id
    where go = callCC $ \finish ->
               withReaderT (\rs -> rs { rsDone = ExitMonadFunction finish }) (runReverseContext routes >> finish Nothing)

reverseURI' :: (CraneApp app, ConEq (RoutesFor app), DeconstructRoute (RoutesFor app)) => RoutesFor app -> CraneMonad app (Maybe T.Text)
reverseURI' route = reversedURI <$> (routes <$> asks crApp)
                                <*> pure route

-- | An unsafe version of reverseURI' that can be used safely so long as the routing structure for `app` is total (contains all `RoutesFor app` constructors).
--   If you're interested in total safety, use `reverseURI'`
reverseURI :: (CraneApp app, ConEq (RoutesFor app), DeconstructRoute (RoutesFor app)) => RoutesFor app -> CraneMonad app T.Text
reverseURI route = fromJust <$> reverseURI' route

-- * Path Components

readMaybe :: Read a => String -> Maybe a
readMaybe x = case reads x of
                (x, ""):_ -> Just x
                _         -> Nothing

instance ToPathComponent Int where
    toPathComponent = fromString . show

instance FromPathComponent Int where
    fromPathComponent = readMaybe . T.unpack

instance ToPathComponent T.Text where
    toPathComponent = id

instance FromPathComponent T.Text where
    fromPathComponent = Just