module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.Maybe(isJust, fromJust, fromMaybe)

-- chatterbott takes String and an array with a tuple of 1 string and then an array of strings and pushes it to IO (I guess it uses random to choose from array)
chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")                   -- like "begin" of session
    botloop                                                                     -- botloop = the loop where he takes question and put answer
  where
    brain = rulesCompile botRules                                               -- Compile rules out of botRules which is the input.
    botloop = do                                                                -- To be written. Creates the brain, ie what we know
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain                                               -- To be written. Chooses the rule to use, uses the brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)       -- combine presenter/answer/prepare to the question.
      if (not . endOfDialog) question then botloop else return ()               -- Have to implement endOfDialog somehow in one method.

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)                               -- Takes a botBrain and turns it into inPutOutput where we have a phrase -> phrase.
stateOfMind brain = do
  rnd <- randomIO :: IO Float
  return $ rulesApply $ (map.map2) (id, (pick rnd)) brain

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply = try.transformationsApply "*" reflect

reflect :: Phrase -> Phrase          
reflect = map $ try $ flip lookup reflections

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile ppairs = (map.map2) (lowerWords, (map lowerWords)) ppairs 
                      where lowerWords = words . map toLower
--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase                                    -- Returns reductionsApply(reductions, X) where X is the phrase used as input.
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase           -- We know that we use "*" as wildcard. transformationsApply works too. 
reductionsApply =  fix . try . transformationsApply "*" id    -- fix makes the function "recursive", so we take the least Phrase that works with the reduction. ie 
                                                              -- "please please help" --> "help" instead of "please help". So it stops first when f x = x.

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute a (b:bs) cs
    | a == b    = cs ++ (substitute a bs cs)       --Merge list of cs and substitution. 
    | otherwise = b : substitute a bs cs

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match w [] [] = Just []
match w [] c = Nothing
match w b [] = Nothing
match w (b:bs) (c:cs)
    | b /= w && b /= c            = Nothing           -- No match
    | b /= w && b == c            = match w bs cs     -- Matching string, keep on going but one step further
    | b == w                      = orElse (singleWildcardMatch (b:bs) (c:cs)) (longerWildcardMatch (b:bs) (c:cs))

-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs)
    | isJust(match wc ps xs)    = Just [x]  -- isJust = bool that is true if Just.  | (old code) ps == xs   = Just [x]
    | otherwise                 = Nothing

longerWildcardMatch (wc:ps) (x:xs) = mmap (x:) (match wc (wc:ps) xs)

-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
-- wildcard, function, inputstring -> (pattern1, pattern2)
-- So basically substitute wildcard from p2, use the function and then match to the wildcard on p1. 
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply w f input (p1, p2) = mmap((substitute w p2) . f) (match w p1 input)

-- Applying a list of patterns until one succeeds
-- maps the function onto the list. Then we fold this with the "orElse" function. So we work till we got a match (starting from right)
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply w f plist input = foldr1 orElse (map (transformationApply w f input) plist)