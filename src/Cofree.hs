{-# LANGUAGE InstanceSigs #-}

module Cofree where

import           Control.Comonad (Comonad (..))


-- | extend :: (f a -> b) -> f a -> f b
-- | substitute section in the function
-- | extend :: (f a -> Cofree f a) -> f a -> f (Cofree f a)
section :: Comonad f => f a -> Cofree f a
section f = extract f :< extend section f

-- | a :< is the obvious first move
-- | Then we need to use `k` somehow, so we do `k a`
-- | That gives us an `f a`
-- | We need a `Cofree f a`, so how can we get there? Recursion of course!
-- | So we have `a` in the `f a` and if we do `coiter k` we have something
-- | that goes from `a` to `Cofree f a`
-- | We also know `f` is a Functor so we can do:
-- | fmap (coiter k) $ k a
coiter :: Functor f => (a -> f a) -> a -> Cofree f a
coiter k a = a :< fmap (coiter k) (k a)

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
