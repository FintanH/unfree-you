{-# LANGUAGE InstanceSigs #-}

module Cofree where

import           Control.Comonad (Comonad (..))

data Cofree f a =
  a :< (f (Cofree f a))

instance Functor f => Functor (Cofree f) where
  fmap f (a :< cofreer) = f a :< fmap (fmap f) cofreer

instance Functor f => Comonad (Cofree f) where
  extract :: Cofree f a -> a
  extract (a :< _) = a

  -- | So we have a function that goes from `Cofree f a` to `b`
  -- | And hmmm we have an `a` which we can't do much with
  -- | And we have cofreer which is `f (Cofree f a)`

  -- | The trick here is that we needed a `b` and we had `(Cofree f a -> b)`
  -- | So we use @ to keep the whole structure as a pattern
  -- | The structure is broken down to a `b` via our `k` function and this
  -- | becomes the head of our new structure!
  extend :: (Cofree f a -> b) -> Cofree f a -> Cofree f b
  extend k co@(a :< cofreer) = k co :< fmap (extend k) cofreer
