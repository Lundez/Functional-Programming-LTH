
--optimalAlignments :: Int -> Int -> Int -> String -> String -> [AlignmentType]

                -- 2a --
type ScoreFunction = (Char -> Char -> Int)

similarityScore :: String -> String -> ScoreFunction -> Int
similarityScore x [] scorer = sum $ map (scorer '-') x
similarityScore [] y scorer = sum $ map (scorer '-') y
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
-- WTF IS THIS xD
-- So we get xs & ys .. From where? xD
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]


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