import Data.Maybe
import Data.List


data Proposition = Var Name
                | Proposition :&: Proposition
                | Proposition :|: Proposition
                | Not Proposition
    deriving(Eq, Show)

type Name = String

vars :: Proposition -> [Name]
vars (Var x)    = [x]
vars (a :&: b)  = vars a ++ vars b
vars (a :|: b)  = vars a ++ vars b
vars (Not a)    = vars a

truthValue :: Proposition -> [(String,Bool)] -> Bool
truthValue (Var x) v    = fromJust(lookup x v)                  --Root-case
truthValue (a :&: b) v  = truthValue a v && truthValue b v
truthValue (a :|: b) v  = truthValue a v || truthValue b v
truthValue (Not a) v    = not(truthValue a v)                      --Turning around "nots"

tautology :: Proposition -> Bool
tautology p = and(map(truthValue p) (allVals (vars p)))         -- and for binding it, map to put truthValue on the allVals possibilities. 

allVals :: [Name] -> [[(Name,Bool)]]                            -- gives all possible combinations, list comprehension <3 
alVals [] = [[]]
allVals (p:ps) = [(p,b):val | val <- allVals ps, b <- [False, True]]

data File = File Name 
        | Dir Name [File] 
        deriving (Eq, Show)

type FileSystem = [File]

search :: FileSystem -> String -> [String]
search files name =
  [ name
  | File name' <- files
  , name == name'
  ] ++
  [ dir ++ "/" ++ path
  | Dir dir files' <- files
  , path <- search files' name
  ]

searchMaybe :: FileSystem -> String -> Maybe String
searchMaybe files name = listToMaybe (search files name)

--searchFile :: File -> Name -> Bool
--searchFile (Dir d fs) x     = map $ (flip searchFile x) fs
--searchFile (Dir d ns) x = id.map(True        --id., map probably? -- TODO
--searchFile (File n) x       = x == n

data Set a = Set [a]
 deriving ( Show )

empty :: Set a
empty = Set []

add :: Eq a => a -> Set a -> Set a
add x (Set xs) 
                | x `elem` xs = Set xs
                | otherwise   = Set(x:xs)

remove :: Eq a => a -> Set a -> Set a
remove x (Set xs)             = Set(xs \\ [x])

combine :: Eq a => Set a -> Set a -> Set a
Set xs `combine` Set ys = Set(xs `union` ys)

member :: Eq a => a -> Set a -> Bool
member x (Set xs) = x `elem` xs

isEmpty :: Set a -> Bool
isEmpty (Set xs) = null xs

instance (Ord a, Ord b) => Ord (a,b) where ...
instance Ord b => Ord [b] where ...