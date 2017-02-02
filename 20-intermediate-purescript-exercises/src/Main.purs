module Main where

import Prelude
import Data.List
import Data.Maybe
import Data.Either
import Data.Tuple

import Unsafe.Coerce (unsafeCoerce)

class Fluffy f where
  furry :: forall a b. (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance fluffyList :: Fluffy List where
  furry = unsafeCoerce "todo"

-- Exercise 2
-- Relative Difficulty: 1
instance fluffyMaybe :: Fluffy Maybe where
  furry = unsafeCoerce "todo"

-- Exercise 3
-- Relative Difficulty: 5
instance fluffyFunction :: Fluffy ((->) t) where
  furry = unsafeCoerce "todo"

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance fluffyEitherLeft :: Fluffy (EitherLeft t) where
  furry = unsafeCoerce "todo"

-- Exercise 5
-- Relative Difficulty: 5
instance fluffyEitherRight :: Fluffy (EitherRight t) where
  furry = unsafeCoerce "todo"

class Misty m where
  banana :: forall a b. (a -> m b) -> m a -> m b
  unicorn :: forall a b. a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: forall a b. (a -> b) -> m a -> m b

defaultMistyFurry' :: forall a b m. Misty m => (a -> b) -> m a -> m b
defaultMistyFurry' = unsafeCoerce "todo"

-- Exercise 7
-- Relative Difficulty: 2
instance mistyList :: Misty List where
  banana = unsafeCoerce "todo"
  unicorn = unsafeCoerce "todo"
  furry' = defaultMistyFurry'

-- Exercise 8
-- Relative Difficulty: 2
instance mistyMaybe :: Misty Maybe where
  banana = unsafeCoerce "todo"
  unicorn = unsafeCoerce "todo"
  furry' = defaultMistyFurry'

-- Exercise 9
-- Relative Difficulty: 6
instance mistyFunc :: Misty ((->) t) where
  banana = unsafeCoerce "todo"
  unicorn = unsafeCoerce "todo"
  furry' = defaultMistyFurry'

-- Exercise 10
-- Relative Difficulty: 6
instance mistyEitherLeft :: Misty (EitherLeft t) where
  banana = unsafeCoerce "todo"
  unicorn = unsafeCoerce "todo"
  furry' = defaultMistyFurry'

-- Exercise 11
-- Relative Difficulty: 6
instance mistyEitherRight :: Misty (EitherRight t) where
  banana = unsafeCoerce "todo"
  unicorn = unsafeCoerce "todo"
  furry' = defaultMistyFurry'

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: forall a m. Misty m => m (m a) -> m a
jellybean = unsafeCoerce "todo"

-- Exercise 13
-- Relative Difficulty: 6
apple :: forall a b m. Misty m => m a -> m (a -> b) -> m b
apple = unsafeCoerce "todo"

-- Exercise 14
-- Relative Difficulty: 6
moppy :: forall a b m. Misty m => List a -> (a -> m b) -> m (List b)
moppy = unsafeCoerce "todo"

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: forall a m. Misty m => List (m a) -> m (List a)
sausage = unsafeCoerce "todo"

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: forall a b c m. Misty m => (a -> b -> c) -> m a -> m b -> m c
banana2 = unsafeCoerce "todo"

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: forall a b c d m. Misty m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 = unsafeCoerce "todo"

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: forall a b c d e m. Misty m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 = unsafeCoerce "todo"

newtype State s a = State {
  state :: (s -> Tuple s a)
}

-- Exercise 19
-- Relative Difficulty: 9
instance fluffyState :: Fluffy (State s) where
  furry = unsafeCoerce "todo"

-- Exercise 20
-- Relative Difficulty: 10
instance mistyState :: Misty (State s) where
  banana = unsafeCoerce "todo"
  unicorn = unsafeCoerce "todo"
  furry' = defaultMistyFurry'
