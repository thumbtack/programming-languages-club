
-- Functors

-- Functor is a type class, just like Read, Show, Ord, or Eq
-- Functors are things that can be "mapped" over

-- The Functor type class is defined as:
{-
class Functor f where
  fmap :: (a -> b) -> f a -> f b
-}

-- In the case of lists, f a means [a], and f b means [b] - f is the list functor
{-
instance Functor [] where
  fmap = map
-}
-- Basically, fmap means the same thing as map when we are talking about lists

fmapList = fmap (+1) [1,2,3]
-- => [2,3,4]

-- But how does mapping work over other type classes?
-- Here's the definition for Maybe:
{-
instance Functor Maybe where
  fmap f mx = case mx of
    Just x  -> Just (f x)
    Nothing -> Nothing
-}

fmapJust = fmap (*3) (Just 5)
-- => Just 15

fmapNothing = fmap (*3) Nothing
-- => Nothing


-- The idea of being able to map over arbitrary types is powerful.
-- Consider mapping over a tree:

data Tree a = Node a (Tree a) (Tree a)
            | EmptyTree
            deriving (Eq, Show)

leaf x = Node x EmptyTree EmptyTree

instance Functor Tree where
  fmap _ EmptyTree           = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

fmapTree = fmap (*5) (Node 10 (leaf 4) (leaf 12))
-- => Node 50 (Node 20 EmptyTree EmptyTree) (Node 60 EmptyTree EmptyTree)

-- The *kind* (not type) of Functor is * -> *. That is, it takes two type parameters.

-- In short, a Functor is a class of types that have a way of applying a function to what they
-- contain (contain in a type sense, not necessarily in a data structure sense)


-- IO is also a functor
instance Functor IO where
  fmap f action = do
    result <- action
    return (f result)

-- This starts to show the importance of the distinction between containing in the type sense
-- and actually containing the data. An IO action doesn't actually contain the result,
-- but its type contains the type of the result.

-- Just like how fmap over a list gives a list and fmap over a Maybe gives a Maybe,
-- fmap over an IO action gives another IO action. The IO action you get out of fmap
-- is the original action with its result run through the function f.

-- Instead of writing
main1 = do
  line <- getLine
  let line' = reverse line
  putStrLn "Reversed: " ++ line'

-- You could write
main2 = do
  line <- fmap reverse getLine
  putStrLn "Reversed: " ++ line

-- fmap creates a new IO action based on the getLine IO action that also reverses the input line.


-- Even functions are Functors!
instance Functor ((->) r) where
  fmap f g = f . g
-- Equivalent to: fmap f g = (\x -> f (g x))

-- In the case of functions, applying a functor just means function composition
-- fmaping a function over another function returns a new function that is the two chained together

-- What on earth is ((->) r)?
-- It is a syntactical oddity, but it just means all but the last part of (r -> a)
-- ((r -> a) means a function taking a parameter of type r and returning one of type a)

-- So this type definition is ignoring the 'r' part of the function type.
-- The instance definition is basically saying, for all functions taking a single parameter of type
-- r, fmap is defined as...

-- Ignoring the input parameter makes sense if you look at what it is actually doing:
-- it is making a new function that takes an input parameter of the same type as the original
-- function (hence it doesn't care about its type), but it does have to specify that the
-- output type of g is the input type of f, and the output type of f is the output type
-- of the new function.


-- The Functor laws

-- 1. fmapping the identity function (id) should never change something
--   (i.e. fmap id x should always == x)

-- 2. fmapping is distributive over function composition.
-- fmap (f . g) should do the same thing as (fmap f) . (fmap g)
-- (fmap (f . g) x === fmap f (fmap g x))


-- Applicative functors

-- Asside: a function with a signature like a -> b -> c is actually a function taking an 'a'
-- that returns a curried function that takes a 'b' and returns a 'c'

-- So far, we have just been mapping functions that take one argument over Functors.
-- What happens if you map a function taking more than one argument, e.g. (*)?
-- What do you get if you run
x1 = fmap (*) (Just 3)
-- ? You get:
x1result = Just (* 3)
-- This idea is the basis of Applicative Functors.

-- If you map `compare` over a list of characters, you get a list of functions Char -> Ordering

x1 = let a = fmap (*) [1,2,3,4]
     in fmap (\ f -> f 10) a
-- x1 = [10,20,30,40]

-- But what if you wanted to map `Just (* 3)` over `Just 5`?
-- here is where applicative functors come in.

class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
