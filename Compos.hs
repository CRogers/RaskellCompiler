{-# LANGUAGE RankNTypes #-} 

module Compos where

import Control.Applicative
import Control.Monad.Identity
import Data.Monoid
import Data.Traversable
import Data.Functor.Compose

-- https://publications.lib.chalmers.se/records/fulltext/local_75172.pdf
class Compos t where
	compos :: Applicative f => (forall a. t a -> f (t a)) -> t c  -> f (t c)

composOp :: Compos t => (forall a. t a -> t a) -> t c -> t c
composOp f = runIdentity . compos (Identity . f)

composFold :: (Compos t, Monoid o) => (forall a. t a -> o) -> t c -> o
composFold f = getConst . compos (Const . f)

composM :: (Compos t, Monad m) => (forall a. t a -> m (t a)) -> t c -> m (t c)
composM f = unwrapMonad . compos (WrapMonad . f)

-- http://stackoverflow.com/questions/18294190/applicative-instance-for-monad-m-monoid-o-m-o
composFoldM :: (Compos t, Monad m, Monoid o) => (forall a. t a -> m o) -> t c -> m o
composFoldM f = liftM getConst . unwrapMonad . getCompose
                . compos (Compose . WrapMonad . liftM Const . f)