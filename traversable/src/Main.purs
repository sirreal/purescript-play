module Main where

import Prelude hiding (min, max)
import Data.Tuple (Tuple(..), uncurry )
import Data.Int (toNumber)
import Global (infinity)
import Control.Alt
import Data.Boolean
import Data.Semiring
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

{-- # Todo (homework):
    (signatures mine)
    * min :: forall a t. Traversable t, Ord a => t a -> a
    * max :: forall a t. Traversable t, Ord a => t a -> a
    * sum :: forall a t. Traversable t, Semiring a => t a -> a
    * lastest :: forall a t. Traversable t => t a -> a
      last element
    * average :: forall a t. Traversable t, EuclideanRing a => t a -> a
--}

newtype Add a = Add a

derive instance newtypeAdd :: Newtype (Add a) _

instance addSemigroup :: Semiring a => Semigroup (Add a) where
  append (Add a) (Add b) = Add (a `add` b)

instance addMonoid :: Semiring a => Monoid (Add a) where
  mempty = Add zero

sum :: forall a t. (Traversable t, Semiring a) => t a -> a
sum = unwrap <<< foldMap Add


newtype Min a = Min a
derive instance newtypeMin :: Newtype (Min a) _

instance minSemigroup :: Semigroup (Min Number) where
  append (Min a) (Min b) = Min (if a < b then a else b)

instance minMonoid :: Monoid (Min Number) where
  mempty = Min infinity

min :: forall a t. Traversable t => t Number -> Number
min = unwrap <<< foldMap Min

newtype Max a = Max a
derive instance newtypeMax :: Newtype (Max a) _

instance maxSemigroup :: Semigroup (Max Number) where
  append (Max a) (Max b) = Max (if a > b then a else b)

instance maxMonoid :: Monoid (Max Number) where
  mempty = Max (-infinity)

max :: forall a t. Traversable t => t Number -> Number
max = unwrap <<< foldMap Max

newtype Average = Average (Tuple Int Int)
instance newtypeAverage :: Newtype Average (Tuple Int Int) where
  wrap = Average
  unwrap (Average tuple) = tuple

mkAvg :: Int -> Average
mkAvg n = Average (Tuple n 1)

instance averageSemigroup :: Semigroup Average where
  append (Average (Tuple sum count)) (Average (Tuple sum' count')) = Average (Tuple (sum + sum') (count + count'))

instance averageMonoid :: Monoid Average where
  mempty = Average (Tuple 0 0)

average :: forall a t. Traversable t => t Int -> Number
average = (\(Tuple a b) -> toNumber a / toNumber b) <<< unwrap <<< foldMap mkAvg

lastest :: forall a t. Traversable t => t a -> Maybe a
lastest = unwrap <<< foldr (\a b -> b `append` First (Just a)) mempty
