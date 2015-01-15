{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
module Web.Crane.Internal where

import Web.Crane.Types

import Control.Monad.Reader
import Control.Monad.Error

monadifyArg :: (ResultsIn o (m b), Monad m) => (a -> o) -> m a -> o
monadifyArg f mkA = makeApplier $ \applyRemaining ->
                    do a <- mkA
                       applyRemaining (f a)

class ResultsIn i o | i -> o where
    makeApplier :: ((i -> o) -> o) -> i

instance ResultsIn (CraneHandler a master) (CraneHandler a master) where
    makeApplier context = context id

instance ResultsIn b o => ResultsIn (a -> b) o where
    makeApplier context a = makeApplier $ \applyRemaining ->
                            context (\f -> applyRemaining (f a))


-- * CraneMonad goodies

mapCraneHandler :: (master -> sub) -> (RoutesFor sub -> RoutesFor master) -> CraneHandler sub supermaster -> CraneHandler master supermaster
mapCraneHandler subsiteFromMaster subRouteToMaster subAction =
    do subApp <- asks (subsiteFromMaster . crApp)
       masterRouteToSuperMaster <- asks crAppRouteToMaster

       let lowerRequestToSubApp request = request { crApp = subApp
                                                  , crAppRouteToMaster = masterRouteToSuperMaster . subRouteToMaster }

       CraneResponseBuilder subResponseBuilder <- mapErrorT (withReaderT lowerRequestToSubApp) subAction

       let masterResponseBuilder = CraneResponseBuilder $
                                   \masterRequest response ->
                                       let subRequest = lowerRequestToSubApp masterRequest
                                       in subResponseBuilder subRequest response
       return masterResponseBuilder

mapAfterResponseActions :: (master -> sub) -> (RoutesFor sub -> RoutesFor master) -> AfterResponseActions sub supermaster -> AfterResponseActions master supermaster
mapAfterResponseActions subsiteFromMaster subRouteToMaster (AfterResponseActions subActions) =
    AfterResponseActions $ \response ->
        do subApp <- asks (subsiteFromMaster . crApp)
           masterRouteToSuperMaster <- asks crAppRouteToMaster

           let lowerRequestToSubApp request = request { crApp = subApp
                                                      , crAppRouteToMaster = masterRouteToSuperMaster . subRouteToMaster}

           mapErrorT (withReaderT lowerRequestToSubApp) (subActions response)