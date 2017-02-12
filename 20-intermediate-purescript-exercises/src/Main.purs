module Main where

import Prelude
import Data.List
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..), fst, snd)

class Fluffy f where
  furry :: forall a b. (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance fluffyList :: Fluffy List where
  furry _ Nil         = Nil
  furry f (Cons x xs) = Cons (f x) (furry f xs)

-- Exercise 2
-- Relative Difficulty: 1
instance fluffyMaybe :: Fluffy Maybe where
  furry _ Nothing = Nothing
  furry f (Just x) = Just (f x)

-- Exercise 3
-- Relative Difficulty: 5
instance fluffyFunction :: Fluffy ((->) t) where
  furry = (<<<)

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance fluffyEitherLeft :: Fluffy (EitherLeft t) where
  furry _ (EitherLeft (Right x)) = EitherLeft (Right x)
  furry f (EitherLeft (Left x)) = EitherLeft (Left (f x))

-- Exercise 5
-- Relative Difficulty: 5
instance fluffyEitherRight :: Fluffy (EitherRight t) where
  furry _ (EitherRight (Left x)) = EitherRight (Left x)
  furry f (EitherRight (Right x)) = EitherRight (Right (f x))

class Misty m where
  banana :: forall a b. (a -> m b) -> m a -> m b
  unicorn :: forall a. a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: forall a b. (a -> b) -> m a -> m b

defaultMistyFurry' :: forall a b m. Misty m => (a -> b) -> m a -> m b
defaultMistyFurry' f = banana (unicorn <<< f)

-- Exercise 7
-- Relative Difficulty: 2
instance mistyList :: Misty List where
  banana = concatMap
  unicorn x = Cons x Nil
  furry' f ma = defaultMistyFurry' f ma

-- Exercise 8
-- Relative Difficulty: 2
instance mistyMaybe :: Misty Maybe where
  banana _ Nothing  = Nothing
  banana f (Just x) = f x
  unicorn = Just
  furry' f ma = defaultMistyFurry' f ma

-- Exercise 9
-- Relative Difficulty: 6
instance mistyFunc :: Misty ((->) t) where
  banana f g = \x -> f (g x) x
  unicorn = const
  furry' f ma = defaultMistyFurry' f ma

-- Exercise 10
-- Relative Difficulty: 6
instance mistyEitherLeft :: Misty (EitherLeft t) where
  banana f (EitherLeft (Left x)) = f x
  banana _ (EitherLeft (Right x)) = EitherLeft (Right x)
  unicorn = EitherLeft <<< Left
  furry' f ma = defaultMistyFurry' f ma

-- Exercise 11
-- Relative Difficulty: 6
instance mistyEitherRight :: Misty (EitherRight t) where
  banana f (EitherRight (Right x)) = f x
  banana _ (EitherRight (Left x)) = EitherRight (Left x)
  unicorn = EitherRight <<< Right
  furry' f ma = defaultMistyFurry' f ma

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: forall a m. Misty m => m (m a) -> m a
jellybean = banana id

-- Exercise 13
-- Relative Difficulty: 6
apple :: forall a b m. Misty m => m a -> m (a -> b) -> m b
apple ma mfab = banana ((flip furry') ma) mfab

-- Exercise 14
-- Relative Difficulty: 6
moppy :: forall a b m. Misty m => List a -> (a -> m b) -> m (List b)
moppy xs f = foldr f' (unicorn Nil) xs
  where
    f' el acc = apple acc (furry' Cons (f el))

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: forall a m. Misty m => List (m a) -> m (List a)
sausage = (flip moppy) (furry' id)

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: forall a b c m. Misty m => (a -> b -> c) -> m a -> m b -> m c
banana2 f ma mb = apple mb (furry' f ma)

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: forall a b c d m. Misty m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 f ma mb mc = apple mc (banana2 f ma mb)

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: forall a b c d e m. Misty m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 f ma mb mc md = apple md (banana3 f ma mb mc)

newtype State s a = State (s -> Tuple s a)

instance newtypeState :: Newtype (State s a) (s -> Tuple s a) where
  wrap = State
  unwrap (State s) = s

-- Exercise 19
-- Relative Difficulty: 9
instance fluffyState :: Fluffy (State s) where
  furry f state = State (\s -> let prev = (unwrap state) s
                               in  Tuple (fst prev) (f (snd prev))
                  )

-- Exercise 20
-- Relative Difficulty: 10
instance mistyState :: Misty (State s) where
  banana f s = State (
    \x -> let prev = unwrap s x
              y = fst prev
              a = snd prev
          in unwrap (f a) y
  )
  unicorn x = State (\s -> Tuple s x)
  furry' f ma = defaultMistyFurry' f ma
