module ListOps
  ( length,
    reverse,
    map,
    filter,
    foldr,
    foldl',
    (++),
    concat,
  )
where

import Prelude hiding
  ( (++),
    concat,
    filter,
    foldr,
    length,
    map,
    reverse,
  )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z0 xs = foldr f' id xs z0
  where
    f' x k z = k $! f z x

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x : xs) = f x (foldr f z xs)

length :: [a] -> Int
length = foldr (\_ acc -> acc + 1) 0

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) <$> f) []

filter :: (a -> Bool) -> [a] -> [a]
filter p =
  foldr (\x acc -> if p x then x : acc else acc) []

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat = foldr (++) []
