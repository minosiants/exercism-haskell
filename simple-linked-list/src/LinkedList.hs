module LinkedList
  ( LinkedList,
    datum,
    fromList,
    isNil,
    new,
    next,
    nil,
    reverseLinkedList,
    toList,
  )
where

data LinkedList a
  = Nil
  | Cons a (LinkedList a)
  deriving (Eq, Show)

datum :: LinkedList a -> a
datum Nil = error "It is Nil"
datum (Cons a _) = a

fromList :: [a] -> LinkedList a
fromList [] = Nil
fromList (x : xs) = (Cons x (fromList xs))

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new x linkedList = Cons x linkedList

next :: LinkedList a -> LinkedList a
next Nil = Nil
next (Cons _ n) = n

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList linkedList = go linkedList Nil
  where 
    go Nil nl = nl
    go (Cons a rest) nl = 
      go rest (new a nl)
toList :: LinkedList a -> [a]
toList Nil = []
toList (Cons a l) = a : (toList l)
