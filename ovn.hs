import System.IO

maxi :: Ord a => a -> a -> a
maxi a b
    | a <= b    = b
    | otherwise = a

-- sumsq n returns 1*1 + 2*2 +..+ n*n

sumsq :: Int -> Int
sumsq n
    | n <= 0        = 0
    | otherwise     = n^2 + sumsq(n-1)

sumsqmap :: Int -> Int
sumsqmap n
    | n <= 0    = 0
    | otherwise = foldl (+) 0 (map(^2)[1..n])

hanoi :: Integer -> Integer
hanoi n
    | n <= 0    = 0
    | otherwise = 1 + 2 * hanoi(n-1)

nextFactor :: Int -> Int -> Int
smallestfactor :: Int -> Int
smallestfactor = nextFactor 1

nextFactor k n
    | k >= n            = n
    | mod n (k+1) == 0  = k+1
    | otherwise         = nextFactor (k+1) n

numFactors :: Int -> Int
numFactors n =
 length $ removeDuplicates $ map (flip nextFactor n) [1..n]

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates [x] = [x]
removeDuplicates (x:xs)
   | elem x xs  = removeDuplicates xs
   | otherwise  = x : removeDuplicates xs

--2.1.5
data Date = Year Month Day
data Month =  January   | February | March    | April
           |  May       | June     | July     | August
           |  September | October  | November | December deriving(Eq)

type Day   = Integer
type Year  = Integer

daysInMonth :: Month -> Integer -> Integer
daysInMonth month year
    | year `mod` 4 == 0 && month == February     = 29
    | month == January || month == March || month == May || month == July || month == August || month == October || month == December = 31
    | month == February     = 28
    | otherwise = 30

--validDate :: Date -> Bool

multiply :: Num a => [a] -> a
multiply = foldr1(*)


substitute :: Eq a => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute x y (z:zs)                   --map(\n -> if n==x then y; else n) z
    | z == x    = y:substitute x y zs
    | otherwise = z:substitute x y zs

--currying: Alla f(x) tar ett argument

substitute2 :: Eq a => a -> a -> [a] -> [a]
substitute2 _ _ [] = []
substitTte2 x y z = map replaceChar z
    where
        replaceChar c
            | c == x    = y
            | otherwise = c

--2.2.3
duplicates :: Eq a => [a] -> Bool
duplicates [] = False
duplicates (z:zs)
    | elem z zs == True  = True
    |  otherwise = duplicates zs

--This wont work. The right side will be true if [] and such.

--2.2.4
pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = [(x,y) | x<-xs, y<-ys]
--This function will create a tuple of all values in list. So a tuple of lists.
pyth n = [(a,b,c) | a<-[1..n], b<-[a..n], c<-[b..n], a^2 + b^2 == c^2]

--2.2.5
remove :: Eq a => a -> [a] -> [a]
remove e [] = []
remove e (z:zs)
    | e == z    = remove e zs
    | otherwise = z : (remove e zs)

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = True
isPermutation (z:zs) xs
    | elem z xs = isPermutation zs (remove z xs)
    | otherwise = False

--2.2.6
--shortestAndLongest :: [[a]] -> [[a]]
shortestAndLongest [] = ([],[])
shortestAndLongest xs = (theShortest xs, theLongest xs)

theShortest (x:xs)
    | null xs || shorter x xs = x
    |otherwise  = theShortest xs
theLongest (x:xs)
    | null xs || longer x xs = x
    | otherwise     = theLongest xs
shorter x xs = (length x) <= (minimum $ map length xs)
longer y ys = (length y) >= (maximum $ map length ys)

mystery xs = foldr (++) [] (map(\y -> [y]) xs)
--xs = list
-- foldr -> Sätter ihop strängar
-- map gör varje sträng i xs till en lista av chars.
-- Bygger en sträng?

main = do
    print $ daysInMonth January 2017
