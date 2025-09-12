import Parser
import Control.Applicative

-- Sequence of well closed parenthesis
-- Synergies: many, some
-- (()(())) -> "(()(()))", ""
-- (())() -> "(())", "()"
paren :: Eq e => Parser Char e [Char]
-- paren = char '(' <:> ( concat <$> many paren) <++> (char ')' <:> pure [])
paren = string "(" <++> ( concat <$> many paren) <++> string ")"

-- Some input for Kingdom of Algorithmia day 7
test0 = "B:=,=,+,-,+,-,+,=,+,-|K:=,+,+,+,-,=,+,=,-,-|J:=,-,=,=,-,+,-,+,+,+|C:+,-,=,-,=,-,=,+,+,+|H:-,+,+,=,+,=,-,=,-,+|F:+,+,+,-,=,-,=,+,-,=|D:-,+,-,=,+,+,+,-,=,=|I:=,+,-,-,=,=,+,+,-,+|E:+,-,+,=,-,+,+,=,-,="
symbol :: Parser Char e Char
symbol = oneOf ['+', '-', '=', 'S']

planId :: Parser Char Int [Char]
planId = some (oneOf ['A'..'Z'])

plan = planId <* (char ':') <~> (sepBy symbol (char ','))
final = sepBy plan (char '|')

-- A natural number 'n', followed by n letters 'a'
nFollowedBynAs :: Eq e => Parser Char e (Int, String)
nFollowedBynAs = do
  n <- natural
  as <- exact (read n) (char 'a')
  return ((read n), as)

-- A sequence of 'a', followed by the same number of 'b'
aFollowedByb :: Eq e => Parser Char e (String, String)
aFollowedByb = do
  as <- some (char 'a')
  bs <- exact (length as) (char 'b')
  return (as, bs)

-- Pasing algebraic expressions with naturals, "+", "*" and "^"
data ASTInt   = Leaf Int | Node String ASTInt ASTInt
  deriving (Eq)
data Operator = Op String

instance Show ASTInt where
  show (Leaf i) = show i
  show (Node op l r) = show op <> "(" <> show l <> ")" <> "(" <> show r <> ")"

instance Treeable ASTInt Operator where
  node lhs (Op o) rhs = Node o lhs rhs

-- sumExpr :: Parser Char Int ASTInt
-- sumExpr = leftBinOpExpr multExpr (Op <$> string "+")
sumExpr :: Parser Char Int [Char]
sumExpr = leftBinOpExpr multExpr (string "+")

-- multExpr :: Parser Char Int ASTInt
-- multExpr = leftBinOpExpr expExpr (Op <$> string "*")
multExpr :: Parser Char Int [Char]
multExpr = leftBinOpExpr expExpr (string "*")

-- expExpr :: Parser Char Int ASTInt
-- expExpr = rightBinOpExpr terM (Op <$> string "^")
expExpr :: Parser Char Int [Char]
expExpr = rightBinOpExpr terM (string "^")

-- terM :: Parser Char Int ASTInt
-- terM = (Leaf . read) <$> natural <|> (string "(" *> sumExpr <* string ")")
terM :: Parser Char Int [Char]
terM = natural <|> (string "(" *> sumExpr <* string ")")

-- Parse a language with miffix operators, based on the following grammar
-- expr   ::= anD | eq | term | fac | if' | closed
-- and'   ::= (and'' &)+ and''
-- and''  ::= eq | closed
-- eq     ::= eq' == eq'
-- eq'    ::= term | fac | closed
-- term   ::= closed ((+ | -) closed)+
-- fac    ::= closed !+
-- if'    ::= (if expr then expr else)+ closed
-- closed ::= b | n | ( expr )
-- Reference: https://www.cse.chalmers.se/~nad/publications/danielsson-norell-mixfix.pdf

expr :: Parser Char Int [Char]
expr = and' <|> eq <|> term <|> fac <|> if' <|> closed

and' :: Parser Char Int [Char]
and' = rightBinOpExpr1 and'' (string "&")

and'' :: Parser Char Int [Char]
and'' = eq <|> closed

eq :: Parser Char Int [Char]
eq = (s' <$> eq') <++> (string "==") <++> (s' <$> eq')

eq' :: Parser Char Int [Char]
eq' = term <|> fac <|> closed

term :: Parser Char Int [Char]
term = leftBinOpExpr1 closed (string "+" <|> string "-")

fac :: Parser Char Int [Char]
fac = closed <++> some (char '!')

-- if'    ::= (if expr then expr else)+ closed
if' :: Parser Char Int [Char]
if' =
  let
    fold_ (ts, t) = foldr (\((((i, e0), t), e1), el) rhs -> s'' (i ++ s' e0 ++ t ++ s' e1 ++ el ++ s' rhs)) t ts
  in fold_ <$> (some (string "if" <~> expr <~> string "then" <~> expr <~> string "else") <~> closed)

-- closed ::= b | n | ( expr )
closed :: Parser Char Int [Char] 
closed = natural <|> (string "(" <++> expr <++> string ")")
