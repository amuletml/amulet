{-# LANGUAGE TupleSections #-}

-- | Represents a series of operations which may be performed on
-- 3-tuples.
module Data.Triple
  ( fst3
  , snd3
  , thd3
  , trimap, first3, second3, third3
  , trimapA, first3A, second3A, third3A
  , firstA, secondA
  ) where

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

trimap :: (a -> a') -> (b -> b') -> (c -> c') -> (a, b, c) -> (a', b', c')
trimap f g h (x, y, z) = (f x, g y, h z)

first3 :: (a -> a') -> (a, b, c) -> (a', b, c)
first3 f = trimap f id id

second3 :: (b -> b') -> (a, b, c) -> (a, b', c)
second3 f = trimap id f id

third3 :: (c -> c') -> (a, b, c) -> (a, b, c')
third3 = trimap id id

trimapA :: Applicative f => (a -> f a') -> (b -> f b') -> (c -> f c') -> (a, b, c) -> f (a', b', c')
trimapA f g h (x, y, z) = (,,) <$> f x <*> g y <*> h z

first3A :: Applicative f => (a -> f a') -> (a, b, c) -> f (a', b, c)
first3A f = trimapA f pure pure

second3A :: Applicative f => (b -> f b') -> (a, b, c) -> f (a, b', c)
second3A f = trimapA pure f pure

third3A :: Applicative f => (c -> f c') -> (a, b, c) -> f (a, b, c')
third3A = trimapA pure pure

firstA :: Applicative f => (a -> f a') -> (a, b) -> f (a', b)
firstA f (x, y) = (,) <$> f x <*> pure y

secondA :: Applicative f => (b -> f b') -> (a, b) -> f (a, b')
secondA f (x, y) = (x,) <$> f y
