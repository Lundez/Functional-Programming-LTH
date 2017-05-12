{- Test for Parser.hs -}
module TestParser where

import Prelude hiding (return, fail)
import Parser

l1 = letter "abc"   {- Just('a',"bc") -}
l2 = letter "123"   {- Nothing -}
l3 = letter ""      {- Nothing -}
checkl1 = l1 == Just('a',"bc")
checkl2 = l2 == Nothing
checkl3 = l3 == Nothing

w1 = spaces "abc"  {- Just("","abc") -}                     ---
w2 = spaces "  \t abc"  {- Just("  \t ","abc") -}           ---
checkw1 = w1 == Just("","abc")
checkw2 = w2 == Just("  \t ","abc")

c1 = chars 2 "abc"          {-  Just ("ab","c")  -}
c2 = chars 0 "ab"          {-  Just ("","ab")  -}
c3 = chars 3 "ab"          {-  Nothing)  -}
checkc1 = c1 == Just ("ab","c")
checkc2 = c2 == Just ("","ab")
checkc3 = c3 == Nothing

r1 = require ":=" ":= 1"     {- Just (":=","1") -}          ---
r2 = require "else" "then"     {- Program error: expecting else near then -}
checkr1 = r1 == Just (":=","1")

a4 = (accept "read" -# word) "read count" {-  Just ("count","") -}  ---
checka4 = a4 == Just ("count","")
