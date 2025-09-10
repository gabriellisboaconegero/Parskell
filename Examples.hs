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

data ASTInt   = Leaf Int | Node String ASTInt ASTInt
  deriving (Eq)
data Operator = Op String

instance Show ASTInt where
  show (Leaf i) = show i
  show (Node op l r) = show op <> "(" <> show l <> ")" <> "(" <> show r <> ")"

instance Treeable ASTInt Operator where
  node lhs (Op o) rhs = Node o lhs rhs

sumExpr :: Parser Char Int ASTInt
sumExpr = leftBinOpExpr multExpr (Op <$> string "+")

multExpr :: Parser Char Int ASTInt
multExpr = leftBinOpExpr expExpr (Op <$> string "*")

expExpr :: Parser Char Int ASTInt
expExpr = rightBinOpExpr term (Op <$> string "^")

term :: Parser Char Int ASTInt
-- term = natural
term = (Leaf . read) <$> natural <|> (string "(" *> sumExpr <* string ")")
-- sumExpr :: Parser Char Int [Char]
-- sumExpr = leftBinOpExpr multExpr (string "+")

-- multExpr :: Parser Char Int [Char]
-- multExpr = leftBinOpExpr expExpr (string "*")

-- expExpr :: Parser Char Int [Char]
-- expExpr = rightBinOpExpr term (string "^")

-- term :: Parser Char Int [Char]
-- -- term = natural
-- term = s'' <$> natural <|> (string "(" *> sumExpr <* string ")")
