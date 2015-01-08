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

instance ResultsIn (CraneHandler a) (CraneHandler a) where
    makeApplier context = context id

instance ResultsIn b o => ResultsIn (a -> b) o where
    makeApplier context a = makeApplier $ \applyRemaining ->
                            context (\f -> applyRemaining (f a))


-- * CraneMonad goodies

mapCraneHandler :: (master -> sub) -> CraneHandler sub -> CraneHandler master
mapCraneHandler subsiteFromMaster subAction =
    do subApp <- asks (subsiteFromMaster . crApp)

       let lowerRequestToSubApp request = request { crApp = subApp }

       CraneResponseBuilder subResponseBuilder <- mapErrorT (withReaderT lowerRequestToSubApp) subAction

       let masterResponseBuilder = CraneResponseBuilder $
                                   \masterRequest response ->
                                       let subRequest = lowerRequestToSubApp masterRequest
                                       in subResponseBuilder subRequest response
       return masterResponseBuilder

mapAfterResponseActions :: (master -> sub) -> AfterResponseActions sub -> AfterResponseActions master
mapAfterResponseActions subsiteFromMaster (AfterResponseActions subActions) =
    AfterResponseActions $ \response ->
        do subApp <- asks (subsiteFromMaster . crApp)
           let lowerRequestToSubApp request = request { crApp = subApp }

           mapErrorT (withReaderT lowerRequestToSubApp) (subActions response)