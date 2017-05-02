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
type AlignmentType      = (String, String)
type AlignScoreFunction = AlignmentType -> Int

alignmentScore :: ScoreFunction -> AlignScoreFunction
alignmentScore scorer (s1, s2) = sum $ zipWith scorer s1 s2

optAlignments :: AlignScoreFunction -> String -> String -> [AlignmentType]
optAlignments scorer [] []              = [("","")]
optAlignments scorer (s1:s1s) []        = attachHeads s1 '-' (optAlignments scorer s1s [])
optAlignments scorer [] (s2:s2s)        = attachHeads '-' s2 (optAlignments scorer [] s2s)
optAlignments scorer (s1:s1s) (s2:s2s)  = maximaBy scorer $ match ++ upperCase ++ underCase
                                        where 
                                        match     = attachHeads s1 s2 (optAlignments scorer s1s s2s)
                                        upperCase = attachHeads '-' s2 (optAlignments scorer (s1:s1s) s2s)
                                        underCase = attachHeads s1 '-' (optAlignments scorer s1s (s2:s2s))

                -- TEST 2 d --
alignScorer = alignmentScore scorer1
test3 = optAlignments alignScorer "writers" "vintner"
expected3 = [("writ-ers", "vintner-"), ("wri-t-ers", "v-intner-"), ("wri-t-ers", "-vintner-")]
check3 = test3 == expected3


                -- 2 e --
outputOptAlignments :: AlignScoreFunction -> String -> String -> IO 
outputOptAlignments scorer (s1:s1s) (s2:s2s)

--similarityScore x [] scorer = sum $ map (scorer '-') x
--similarityScore [] y scorer = sum $ map (scorer '-') y
--similarityScore (s1:s1s) (s2:s2s) scorer = maximum [similarityScore s1s s2s scorer + scorer s1 s2, 
--                                             similarityScore s1s (s2:s2s) scorer + scorer s1 '-' , 
--                                             similarityScore (s1:s1s) s2s scorer + scorer '-' s2]



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