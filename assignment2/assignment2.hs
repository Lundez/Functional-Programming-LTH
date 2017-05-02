import Data.List(elemIndices)
import Data.Maybe
--optimalAlignments :: Int -> Int -> Int -> String -> String -> [AlignmentType]

                -- 2a --
type ScoreFunction = (Char -> Char -> Int)

similarityScore :: String -> String -> ScoreFunction -> Int
similarityScore s1 [] scorer = sum $ map (scorer '-') s1
similarityScore [] s2 scorer = sum $ map (scorer '-') s2
similarityScore (s1:s1s) (s2:s2s) scorer = maximum [similarityScore s1s s2s scorer + scorer s1 s2, 
                                             similarityScore s1s (s2:s2s) scorer + scorer s1 '-' , 
                                             similarityScore (s1:s1s) s2s scorer + scorer '-' s2]

-- calculate similarity, first get the scoreFn by doing: score 0 1 (-1). Then you'll have a (Char -> Char -> Int) function
score :: Int -> Int -> Int -> Char -> Char -> Int
score scoreSpace scoreMatch scoreMismatch x y
                                            | x == '-' || y == '-'      = scoreSpace
                                            | x == y                    = scoreMatch
                                            | otherwise                 = scoreMismatch
                -- TEST 2a --
scorer1 = score (-1) 0 (-1)
s1      = "writers"
s2      = "vintner"
test1   = similarityScore s1 s2 scorer1
check1  = test1 == (-5)

                -- 2b --
-- So basically we got tuple (x,y) and we append h1,h2 as headers to this tuple and put it in a list and then return
-- ie. attachHeads 'h' 'd' [("ej, å")] returns [("hej", "då")]
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

                -- 2c --
maximaBy :: Ord b => (a->b) -> [a] -> [a]
maximaBy valueFcn xs = map (xs !!)  indexesOfMaxVals
                     where 
                     valuesXs         = map valueFcn xs
                     maxValue         = maximum valuesXs
                     indexesOfMaxVals = elemIndices maxValue valuesXs 

                -- TEST 2c --
test2   = maximaBy length ["cs", "efd", "lth", "it"]
check2  = test2 == ["efd", "lth"]

                -- 2d --
type AlignmentType = (String, String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments s1 s2

similarityScore x [] scorer = sum $ map (scorer '-') x
similarityScore [] y scorer = sum $ map (scorer '-') y
similarityScore (s1:s1s) (s2:s2s) scorer = maximum [similarityScore s1s s2s scorer + scorer s1 s2, 
                                             similarityScore s1s (s2:s2s) scorer + scorer s1 '-' , 
                                             similarityScore (s1:s1s) s2s scorer + scorer '-' s2]



mcsLength :: Eq a => [a] -> [a] -> Int
mcsLength xs ys = mcsLen (length xs) (length ys)
  where
    mcsLen i j = mcsTable!!i!!j
    mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]
       
    mcsEntry :: Int -> Int -> Int
    mcsEntry _ 0 = 0
    mcsEntry 0 _ = 0
    mcsEntry i j
      | x == y    = 1 + mcsLen (i-1) (j-1)
      | otherwise = max (mcsLen i (j-1)) 
                        (mcsLen (i-1) j)
      where
         x = xs!!(i-1)
         y = ys!!(j-1)