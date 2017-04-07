module Utilities where

map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)    --Maps 2 functions on two variables and puts it in a tuple
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

mmap :: (a -> b) -> Maybe a -> Maybe b          --Uses map so it puts Just around everything :) Read it as MaybeMap. ;) 
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

orElse :: Maybe a -> Maybe a -> Maybe a         --OrElse onto two maybes. If there is Nothing it is nothing, Just gives just
orElse Nothing  x  = x
orElse (Just a) _  = Just a

try :: (a -> Maybe a) -> a -> a                 -- Uuh, function that takes a and makes it a Maybe. Then results in a value
try f x = maybe x id (f x)

fix :: Eq a => (a -> a) -> a -> a               -- function + variable --> f(x) = x -> x, otherwise go down a level till correct
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

pick :: RealFrac r => r -> [a] -> a             -- Get a real frac? I DONT KNOW
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs