import Parser
import Control.Applicative

-- Sequence of well closed parenthesis
-- Synergies: many, some
-- (()(())) -> "(()(()))", ""
-- (())() -> "(())", "()"
paren :: Eq e => Parser Char e [Char]
paren = char '(' <:> ( concat <$> many paren) <++> (char ')' <:> pure [])

-- A natural number 'n', followed by n letters 'a'
nFollowedBynAs :: Eq e => Parser Char e (Int, String)
nFollowedBynAs = do
  n <- natural
  as <- exact n (char 'a')
  return (n, as)

-- A sequence of 'a', followed by the same number of 'b'
aFollowedByb :: Eq e => Parser Char e (String, String)
aFollowedByb = do
  as <- some (char 'a')
  bs <- exact (length as) (char 'b')
  return (as, bs)

