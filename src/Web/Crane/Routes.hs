{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, GeneralizedNewtypeDeriving, RankNTypes, DoAndIfThenElse, GADTs, KindSignatures, FlexibleContexts, TupleSections, ScopedTypeVariables #-}
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
import Data.Typeable
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T

import GHC.Generics

import Network.Wai

import Unsafe.Coerce

import Web.Cookie (parseCookies)

-- * Path Components

data ReverseState app master = ReverseState
                             { rsRoute :: RoutesFor app
                             , rsApp   :: app
                             , rsMaster :: master
                             , rsDone  :: ExitMonadFunction (ReverseContextM app master) (Maybe T.Text) }

type ReversePathState = [GenericPathComponent]

data ExitMonadFunction m a where
    ExitMonadFunction :: (a -> m r) -> ExitMonadFunction m a

type ReverseContextM app master = ReaderT (ReverseState app master) (Cont (Maybe T.Text))

newtype ReverseContext app master a = ReverseContext { runReverseContext :: ReverseContextM app master a }
    deriving (MonadReader (ReverseState app master), MonadCont, Monad, Applicative, Functor)

newtype ReversePathArrow app master i a = ReversePathArrow { runReversePath :: (T.Text, ReversePathState) -> (T.Text, ReversePathState) }

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
                                        case runRouteDispatch (runPathParser parsePath) (pathConstructor, state) of
                                          Left err -> throwError err
                                          Right (x, state') -> case pathComponents state' of
                                                                 []   -> return x
                                                                 [""] -> return x
                                                                 _    -> throwError NoActionForRoute
    mount pathConstructor parsePath = ActionDispatch $
                                      do state <- ask
                                         case runRouteDispatch (runPathParser parsePath) (pathConstructor, state) of
                                           Left err -> throwError err

                                           -- If this succeeds, then we need to parse the remaining components using the subsite's route parser
                                           Right (x, state') ->
                                               do let subsite = subsiteFromMaster masterRoute (rpsApp state) -- masterRoute (rpsApp state)
                                                      subsiteRps = state' { rpsApp = subsite
                                                                          , pathComponents = pathComponents state' }

                                                      dispatch = routes subsite
                                                      masterRoute = x undefined -- This is used as input to subsiteFromMaster to get the right subsite instance

                                                      subsiteParseResult = runReader (runErrorT (runActionDispatch dispatch)) subsiteRps

                                                      -- -- Force the type system into getting the right subsite type
                                                      -- subsiteFromMaster' :: RouteDispatch app master a (RoutesFor sub -> RoutesFor app) -> sub
                                                      -- subsiteFromMaster' _ = subsiteFromMaster (proxyFor (rpsMaster state)) masterRoute (rpsApp state)

                                                  case subsiteParseResult of
                                                    Left err -> throwError err
                                                    Right (subHandler, subAfterResponseActions) ->
                                                        let masterHandler = mapCraneHandler (subsiteFromMaster masterRoute) x subHandler
                                                            masterAfterResponseActions = mapAfterResponseActions (subsiteFromMaster masterRoute) x subAfterResponseActions
                                                        in return (masterHandler, masterAfterResponseActions)

(*-) :: ConstructHandler app master (CraneHandler app master) ->
        ConstructHandler app master (CraneHandler app master) ->
        ConstructHandler app master (CraneHandler app master)
ConstructHandler a *- ConstructHandler b = ConstructHandler (catchError a (const b))
infixl 5 *-

instance Category (RouteDispatch app master) where
    id = RouteDispatch Right
    RouteDispatch a . RouteDispatch b = RouteDispatch $ \(i, state) -> case b (i, state) of
                                                                         Left err -> Left err
                                                                         Right (i', state') -> a (i', state')

instance Arrow (RouteDispatch app master) where
    arr f = RouteDispatch $ \(i, state) -> Right (f i, state)
    first (RouteDispatch f) = RouteDispatch $ \((b, d), state) -> case f (b, state) of
                                                                    Left err -> Left err
                                                                    Right (b', state') -> Right ((b', d), state')

instance ArrowChoice (RouteDispatch app master) where
    left (RouteDispatch f) =
        RouteDispatch $ \(i, state) ->
            case i of
              Left x  -> case f (x, state) of
                           Left err -> Left err
                           Right (x', state') -> Right (Left x', state')
              Right x -> Right (Right x, state)


root :: (PathContext pc, Category (pc app master)) => PathParser pc app master a a
root = PathParser id

txt :: ( PathContext pc, Category (pc app master)
       , Arrow (pc app master), ArrowChoice (pc app master) ) =>
       T.Text -> PathParser pc app master a a
txt x = PathParser ((popPathComponent (Left x) &&& id) >>>
                    arr (\(component, input) ->
                             case component of
                               Just c  -> if c == x then Right input else Left (CouldNotMatchComponent x c)
                               Nothing -> Left (CouldNotMatchComponent x "")) >>>
                    (routeParseError ||| id))

var :: ( PathContext pc, Category (pc app master)
       , Arrow (pc app master), ArrowChoice (pc app master)
       , FromPathComponent a, ToPathComponent a ) =>
       PathParser pc app master (a -> b) b
var = PathParser ((parseComponent &&& id) >>> -- We need to do a >>> (parseComponent &&& id) so that a gets to parse first. Otherwise, parseComponent and a both get the same thing
                  arr (\(parsedArg, f) -> f parsedArg))
    where parseComponent = popPathComponent (Right toPathComponent) >>>
                           arr (\c -> case fromPathComponent =<< c of
                                        Just x -> Right x
                                        _      -> Left (UnableToInterpret (show c))) >>>
                           (routeParseError ||| id)

(*/) :: Category (pc app master) => PathParser pc app master a b -> PathParser pc app master b c -> PathParser pc app master a c
PathParser a */ PathParser b = PathParser (a >>> b)
infixl 4 */

instance (PathContext pc, Category (pc app master), Arrow (pc app master), ArrowChoice (pc app master)) => IsString (PathParser pc app master a a) where
    fromString x = txt (T.pack x)

lookupActionForRoute :: CraneApp app => app -> Request -> Either RouteParseError (CraneHandler app app, AfterResponseActions app app)
lookupActionForRoute app req =
    let routeParseState = RouteParseState { pathComponents = pathInfo req
                                          , rpsWaiRequest = req
                                          , rpsParsedCookies = case cookieHeader of
                                                                 Just cookieHeader -> parseCookies cookieHeader
                                                                 Nothing -> []
                                          , rpsApp = app
                                          , rpsMaster = app}

        cookieHeader = lookup cookieHeaderName (requestHeaders req)

        dispatch = routes app
    in runReader (runErrorT (runActionDispatch dispatch)) routeParseState

-- * URI Reversal

finish :: Maybe T.Text -> ReverseContextM app master a
finish result = do st <- ask
                   case st of
                     -- XXX unsafeCoerce...
                     ReverseState { rsDone = ExitMonadFunction finish' } -> (unsafeCoerce finish') result

instance Category (ReversePathArrow app master) where
    id = ReversePathArrow id
    ReversePathArrow a . ReversePathArrow b = ReversePathArrow (a . b)

instance Arrow (ReversePathArrow app master) where
    arr f = ReversePathArrow id
    first (ReversePathArrow f) = ReversePathArrow f

instance ArrowChoice (ReversePathArrow app master) where
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
                                  then finish (Just (fst (runReversePath (runPathParser parseRoute) ("", deconstructRoute expRoute))))
                                  else finish Nothing
    mount routeCon parseRoute = ReverseContext $
                                do let route = getRoute routeCon
                                   expRoute <- asks rsRoute
                                   app      <- asks rsApp
                                   master   <- asks rsMaster
                                   if route @== expRoute
                                   then do let (masterPrefix, remainingComponents) = runReversePath (runPathParser parseRoute) ("", deconstructRoute expRoute)

                                               subApp' :: IsCraneSubsite app sub => PathParser ReversePathArrow app master a (RoutesFor sub -> RoutesFor app) -> RoutesFor app -> app -> sub
                                               subApp' _ = subsiteFromMaster

                                               subApp = subApp' parseRoute expRoute app

                                               subAppRoutes = routes subApp
                                           case remainingComponents of
                                             [GenericPathComponent c] ->
                                                 case cast c of
                                                   Nothing -> finish Nothing
                                                   Just subRoute -> case reversedURI subAppRoutes subApp subRoute of
                                                                      Nothing -> finish Nothing
                                                                      Just subRoute -> finish (Just $ masterPrefix `T.append` subRoute)
                                             _ -> finish Nothing
                                   else finish Nothing

reversedURI :: (ConEq (RoutesFor app), DeconstructRoute (RoutesFor app)) => ReverseContext app app a -> app -> RoutesFor app -> Maybe T.Text
reversedURI routes app r = runCont (runReaderT go (ReverseState r app app undefined)) id
    where go = callCC $ \finish ->
               withReaderT (\rs -> rs { rsDone = ExitMonadFunction finish }) (runReverseContext routes >> finish Nothing)

masterRoutes :: (CraneApp master, ActionContext ac pc, Arrow (pc master master), ArrowChoice (pc master master)) =>
                ac master master (CraneHandler master master, AfterResponseActions master master) -> ac master master (CraneHandler master master, AfterResponseActions master master)
masterRoutes = id

reverseURI' :: ( CraneApp master, CraneApp app ) => RoutesFor app -> CraneMonad app master (Maybe T.Text)
reverseURI' route = reversedURI <$> (masterRoutes . routes <$> asks crMaster) -- We always reverse the route in the master's context
                                <*> asks crMaster
                                <*> (asks crAppRouteToMaster <*> pure route) -- which means we have to convert the local route into a master route

-- | An unsafe version of reverseURI' that can be used safely so long as the routing structure for `app` is total (contains all `RoutesFor app` constructors).
--   If you're interested in total safety, use `reverseURI'`
reverseURI :: (CraneApp master, CraneApp app) => RoutesFor app -> CraneMonad app master T.Text
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