module Lang where
import Parser
import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe

-- ===================================
-- INT ::= [-]DIGIT+
-- ID ::= ALPHA(ALPHANUM | '_')*
-- STRINGLIT ::= '"' [^'"' '\n']* '"'
-- BUILTINFUN ::= 
-- final ::= expr (' '* ';' ' '* expr)* (' '* ';' ' '*)?
-- expr ::= lambda | funcAppP | sumTerm | closed
-- lambda ::= ' '* 'lamb' ' '* ID ' '* '=>' ' '* expr
-- sumTerm ::= multTerm (' '* '+' ' '* multTerm)+
-- multTerm ::= funcApp' (' '* '*' ' '* funcApp')+
-- funcApp' ::= funcAppP | closed
-- funcApp ::= closed (' '* '.' ' '* closed)+
-- closed ::= ID | INT | STRINGLIT | | '(' expr ')'

type Identifier = String

data ParseError = ReservedWord String
  | Generic
  deriving (Show, Eq)

-- ======================= Expressions ========================
data Expr = Var Identifier
  | IntegerLiteral Int
  | StringLiteral String
  | FunctionApp Expr Expr
  | Op Operator
  | LambdaAbstraction Identifier Expr
  deriving (Eq)

instance Show Expr where
  show e = surround "(" ")" $ show' e
    where
      show' (Var v)                 = show v
      show' (IntegerLiteral i)      = show i
      show' (StringLiteral s)       = show s
      show' (FunctionApp f e)       = ":" ++ show f ++ ": " ++ show e
      show' (Op o)                  = show o
      show' (LambdaAbstraction i e) = show i ++ "-> " ++ show e

instance Treeable Expr String where
  node lhs "." rhs = FunctionApp lhs rhs
  node lhs op rhs = FunctionApp (FunctionApp (toBuiltInOp op) lhs) rhs
-- ======================= Expressions ========================

-- ======================= Rserverd Words =====================
reservedWords :: [String]
reservedWords = ["lamb"]
-- ======================= Rserverd Words =====================

-- ======================= Binary Operators ========================
data Operator = Sum | Sub
  | Mult | Div
  | And | Or | Equals
  | Null
  deriving (Show, Eq)

data Associativity = LeftAssoc | RightAssoc
  deriving (Show, Eq)

instance Ord Associativity where
  compare LeftAssoc RightAssoc = LT
  compare RightAssoc LeftAssoc = GT
  compare a b                  = EQ

data BinaryOperatorDef = BinOpDef
  { op    :: Operator,
    str   :: String,
    prec  :: Int,
    assoc :: Associativity
  }
  deriving (Show, Eq)

instance Ord BinaryOperatorDef where
  compare a b = 
    let
      pa = prec a
      pb = prec b
      aa = assoc a
      ab = assoc b
    in if pa == pb then compare aa ab else compare pa pb 

builtInBinaryOperators :: [BinaryOperatorDef]
builtInBinaryOperators = [
  BinOpDef Sum    "+"  10 LeftAssoc,
  BinOpDef Sub    "-"  10 LeftAssoc,
  BinOpDef And    "&&" 07 LeftAssoc,
  BinOpDef Or     "||" 06 LeftAssoc,
  BinOpDef Equals "==" 08 LeftAssoc,
  BinOpDef Mult   "*"  15 LeftAssoc,
  BinOpDef Div    "/"  15 LeftAssoc]

-- Group all binaryoperators in groups that contain the same precedence and associativity
-- ordered by precedence than associativity
groupedBinaryOps :: [[BinaryOperatorDef]]
groupedBinaryOps = 
  let
    sorted = sortBy (flip compare) builtInBinaryOperators
    grouped = groupBy (\a b -> (a >= b) && (a <= b)) sorted
  in grouped

binaryOperatorP :: [BinaryOperatorDef] -> Parser Char ParseError String
binaryOperatorP = choice . map (\bop -> string $ str bop)

binaryOperatorsTableP :: Parser Char ParseError Expr -> [[BinaryOperatorDef]] -> [Parser Char ParseError Expr]
binaryOperatorsTableP closed (x:xs) = foldl foldF [leftBinOpExpr closed (toStrP x)] xs
  where
    foldF acc x
      | assoc (head x) == LeftAssoc = (leftBinOpExpr (head acc) (toStrP x)):acc
      | assoc (head x) == RightAssoc = (rightBinOpExpr (head acc) (toStrP x)):acc
    toStrP = betWs . binaryOperatorP
-- ======================= Binary Operators ========================

-- ============== UTILS ====================
ws :: Eq e => Parser Char e ()
ws = const () <$> many (satisfy isSpace)

betWs :: Parser Char ParseError a -> Parser Char ParseError a
betWs p = betweenC ws p ws

parentsC :: Parser Char ParseError a -> Parser Char ParseError a
parentsC p = betweenC (betWs $ char '(') p (betWs $ char ')')

toBuiltInOp :: String -> Expr
toBuiltInOp a
  | isNothing f =  error $ "Expected operator, but got " ++ show a 
  | otherwise   = Op $ fromMaybe Null $ do bop <- f; return (op bop)
  where
    f = find (\bop -> str bop == a) builtInBinaryOperators
-- ============== UTILS ====================

-- ============== EXPR PARSERS ====================
finalP :: Parser Char ParseError [Expr]
finalP = sepEndBy exprP (betWs $ char ';')

exprP :: Parser Char ParseError Expr
exprP = lambdaP <|> term <|> closedP

lambdaP :: Parser Char ParseError Expr
lambdaP = LambdaAbstraction <$> (string "lamb" *> ws *> identifierP <* ws <* string "=>" <* ws) <*> exprP

term :: Parser Char ParseError Expr
term = head $ binaryOperatorsTableP funcAppP groupedBinaryOps

funcAppP :: Parser Char ParseError Expr
funcAppP = leftBinOpExpr1 funcAppP' (betWs $ string ".") <|> closedP

funcAppP' :: Parser Char ParseError Expr
funcAppP' = closedP <|> (toBuiltInOp <$> builtInP)

closedP :: Parser Char ParseError Expr
closedP = (IntegerLiteral <$> literalIntegerP) <|> (Var <$> identifierP) <|> (StringLiteral <$> stringLitP) <|> parentsC exprP
-- ============== EXPR PARSERS ====================

-- ============== GENERAL PARSERS ====================
identifierP :: Parser Char ParseError Identifier
identifierP = do
  r <- satisfy isAlpha <:> many (satisfy isAlphaNum <|> char '_')
  if r `elem` reservedWords then
    buildParserWithError $ ReservedWord r
  else
    return r

literalIntegerP :: Parser Char ParseError Int
literalIntegerP = read <$> integer

stringLitP :: Parser Char ParseError String
stringLitP = betweenC (char '"') (many $ satisfy (\x -> x /= '"' && x /= '\n')) (char '"')

-- Remember to modify toBuiltInOp if builtInP is modified
builtInP :: Parser Char ParseError String
builtInP = binaryOperatorP builtInBinaryOperators
