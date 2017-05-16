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
    While Expr.T Statement|
    Read String |
    Comment String |
    Write Expr.T
    deriving Show

-- statement ::= variable ':=' expr ';'
            -- | 'skip' ';'
            -- | 'begin' statements 'end'
            -- | 'if' expr 'then' statement 'else' statement
            -- | 'while' expr 'do' statement
            -- | 'read' variable ';'
            -- | 'write' expr ';'

assignment          = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e)     = Assignment v e

ifStatement         = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf ((e, s1), s2) = If e s1 s2

beginStatement      = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin s1       = Begin s1

skipStatement       = accept "skip" -# require ";" >-> buildSkip
buildSkip s         = Skip

whileStatement      = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e, s1)  = While e s1

readStatement       = accept "read" -# word #- require ";"  >-> buildRead
buildRead s1        = Read s1

writeStatement      = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e        = Write e

commentStatement    = accept "--" -# comment #- require "\n" >-> buildComment
buildComment a      = Comment a

showStmt :: Int -> Statement -> String
showStmt n (Assignment v e) = (replicate n '\t') ++ v ++ " := " ++ (Expr.toString e) ++ ";\n"
showStmt n (Skip)           = (replicate n '\t') ++ "skip;\n"
showStmt n (If e s1 s2)     = (replicate n '\t') ++ "if " ++ (Expr.toString e) ++ " then\n" ++ showStmt (n+1) s1 ++ (replicate n '\t') ++ "else\n" ++ showStmt (n+1) s2
showStmt n (Begin s1s)      = (replicate n '\t') ++ "begin\n" ++ concat(map (showStmt (n+1)) s1s) ++ (replicate n '\t') ++ "end\n"
showStmt n (While e s1)     = (replicate n '\t') ++ "while " ++ (Expr.toString e) ++ (replicate n '\t') ++ " do\n" ++ showStmt (n+1) s1
showStmt n (Read s1)        = (replicate n '\t') ++ "read " ++ s1 ++ ";\n"
showStmt n (Write e)        = (replicate n '\t') ++ "write " ++ (Expr.toString e) ++ ";" ++ "\n"
showStmt n (Comment s)      = (replicate n '\t') ++ "--" ++ s ++ "\n"


exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] dict input = []
exec (If cond thenStmts elseStmts: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (Assignment v e: stmts) dict input = exec stmts dictNew input
                                   where
                                       dictNew = Dictionary.insert (v, Expr.value e dict) dict
exec (Skip: stmts) dict input = exec stmts dict input
exec (Begin s1 : stmts) dict input = exec (s1 ++ stmts) dict input
exec (While e s: stmts) dict input =
    if(Expr.value e dict)> 0
    then exec (s : (While e s) : stmts) dict input
    else exec stmts dict input
exec (Read s1 : stmts) dict (input:rest) = exec stmts (Dictionary.insert(s1, input) dict) rest
exec (Write e : stmts) dict input = (Expr.value e dict) : exec stmts dict input
exec (Comment a: stmts) dict input = exec stmts dict input

instance Parse Statement where
  parse = assignment ! ifStatement ! beginStatement ! skipStatement ! whileStatement ! readStatement ! writeStatement ! commentStatement
  toString = showStmt 0
