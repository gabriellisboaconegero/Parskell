{-# LANGUAGE MultilineStrings #-}
module Tests where
import Lang
import Parser

test1 = """
lamb a =>
  lamb b =>
    a + b;
"""

test2 = """
lamb c =>
  lamb d =>
    c + d + e * f + g;
""" 

test3 = """
lamb f =>
  +. (/. f. -1). 5;

lamb f =>
  +. (-. (-. f. -4). 1). 5;
"""

test4 = """
lamb f =>
  lamb a => +. a;

lamb f =>
  lamb a =>
  +. (f. (g. a)). 5;
"""

test5 = """
lamb f =>
  x. 4. (lamb a => f. a);
"""

test6 = """
f. a + g. 5 == 0 && x || a - 6 * 4 && 34 == k;
"""

test7 = """
lamb s => lamb s_ => +. s. (+. "pedro + lucas". s_);
"""

test8 = """
a == b ?
  print. "foo"
:
  print. "bar"
"""

test9 = """
a == b ?
  f. a ?
    1
  :
    2
: b ?
    print. a
  :
    print. b
"""
