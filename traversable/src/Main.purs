module Main where

import Prelude
import Data.List
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Endo
import Data.Newtype hiding (traverse)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"

class Functor t <= Traversable t where
  traverse :: forall f a b. Applicative f => (a -> f b) -> t a -> f (t b)

newtype Const a b = Const a

derive instance newtypeConst :: Newtype (Const a b) _

instance funtorConstant :: Functor (Const m) where
  map _ (Const m) = Const m

instance traversableList :: Traversable List where
  traverse _ Nil         = pure Nil
  traverse f (Cons x xs) = (Cons) <$> f x <*> traverse f xs

instance monoidApply :: Monoid m => Apply (Const m) where
  apply (Const m) (Const m') = Const (m <> m')

instance monoidApplicative :: Monoid m => Applicative (Const m) where
  pure _ = Const mempty

foldMap :: forall a f m. (Traversable f, Monoid m) => (a -> m) -> f a -> m
foldMap f = unwrap <<< traverse (Const <<< f)

foldr :: forall a b t. Traversable t => (a -> b -> b) -> b -> t a -> b
foldr f seed ta = unwrap (foldMap (Endo <<< f) ta) seed
