module Main where

import Prelude
import Control.Alt
import Data.Boolean
import Data.Maybe
import Data.List hiding (foldr, foldl, foldMap)
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
  apply (Const a) (Const b) = Const (a <> b)

instance monoidApplicative :: Monoid m => Applicative (Const m) where
  pure _ = Const mempty

foldMap :: forall a f m. (Traversable f, Monoid m) => (a -> m) -> f a -> m
foldMap f = unwrap <<< traverse (Const <<< f)

foldr :: forall a b t. Traversable t => (a -> b -> b) -> b -> t a -> b
foldr f seed ta = unwrap (foldMap (Endo <<< f) ta) seed

foldl :: forall a b t. Traversable t => (b -> a -> b) -> b -> t a -> b
foldl f seed ta = unwrap (foldMap (Endo <<< flip f) ta) seed

newtype First a = First (Maybe a)

derive instance newtypeFirst :: Newtype (First a) _

instance firstSemigroup :: Semigroup (First a) where
  append (First a) (First b) = First (a <|> b)

instance firstMonoid :: Monoid (First a) where
  mempty = First Nothing


find :: forall a t. Traversable t => (a -> Boolean) -> t a -> Maybe a
find f ta = unwrap (foldMap go ta)
  where
    go a = if f a then First (Just a) else mempty

newtype Or = Or Boolean

derive instance newtypeOr :: Newtype (Or) _

instance orSemigroup :: Semigroup Or where
  append (Or a) (Or b) = Or (a || b)

instance orMonoid :: Monoid Or where
  mempty = Or false

any :: forall a t. Traversable t => (a -> Boolean) -> t a -> Boolean
any f = unwrap <<< foldMap (Or <<< f)

newtype And = And Boolean

derive instance newtypeAnd :: Newtype (And) _

instance andSemigroup :: Semigroup And where
  append (And a) (And b) = And (a && b)

instance andMonoid :: Monoid And where
  mempty = And true

all :: forall a t. Traversable t => (a -> Boolean) -> t a -> Boolean
all f = unwrap <<< foldMap (And <<< f)
