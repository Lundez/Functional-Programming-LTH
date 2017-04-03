import Data.Maybe
import Data.List


data Proposition = Var Name
                | Proposition :&: Proposition
                | Proposition :|: Proposition
                | Not Proposition
    deriving(Eq, Show)

type Name = String

vars :: Proposition -> [Name]
vars (Var x) = [x]
vars (a :&: b)  = vars a `union` vars b
vars (a :|: b)  = vars a `union` vars b
vars (Not a)    = vars a

truthValue :: Proposition -> [(String,Bool)] -> Bool
