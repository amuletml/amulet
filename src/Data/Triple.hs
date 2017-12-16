module Data.Triple
  ( fst3
  , snd3
  , thd3
  , trimap
  , first3, second3, third3
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
