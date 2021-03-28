module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef
import Data.List

data Deque a
  = Deque (IORef [a])
  deriving (Eq)

mkDeque :: IO (Deque a)
mkDeque = do
  ref <- newIORef []
  return $ Deque ref

pop :: Deque a -> IO (Maybe a)
pop (Deque ref) = do
  xs <- readIORef ref
  let res = if null xs then Nothing else Just $ last xs
  modifyIORef ref init
  return res

push :: Deque a -> a -> IO ()
push (Deque ref) x = do
    modifyIORef ref (++[x])


unshift :: Deque a -> a -> IO ()
unshift (Deque ref) x = do
   modifyIORef ref (x :)


shift :: Deque a -> IO (Maybe a)
shift (Deque ref) = do 
    xs <- readIORef ref
    let res = if null xs then Nothing else Just $ head xs 
    modifyIORef ref tail 
    return res 
