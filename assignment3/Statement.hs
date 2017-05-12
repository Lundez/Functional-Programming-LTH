module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Begin [Statement] |
    Skip |
    While Expr.T |
    Read String |
    Write Expr.T
    deriving Show

-- statement ::= variable ':=' expr ';'
            -- | 'skip' ';'
            -- | 'begin' statements 'end'
            -- | 'if' expr 'then' statement 'else' statement
            -- | 'while' expr 'do' statement
            -- | 'read' variable ';'
            -- | 'write' expr ';'

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

ifStatement= Expr.parse #- require "then" # Statement.parse #- require "else" # Statement.parse >-> buildIf
buildIf (e, s1, s2) = If e s1 s2

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input


instance Parse Statement where
  parse = error "Statement.parse not implemented"
  toString = error "Statement.toString not implemented"
