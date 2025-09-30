{-# LANGUAGE MultilineStrings #-}
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
  | Op OperatorName
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

instance Treeable Expr Operator where
  node lhs (FuncApp, _) rhs = FunctionApp lhs rhs
  node lhs (opName, mixfixExprs) rhs = foldl (\acc x -> FunctionApp acc x) (Op opName) ([lhs] ++ mixfixExprs ++ [rhs])
-- ======================= Expressions ========================

-- ======================= Rserverd Words =====================
reservedWords :: [String]
reservedWords = ["lamb"]
-- ======================= Rserverd Words =====================

-- ======================= Binary Operators ========================
data OperatorName = Sum | Sub
  | Mult | Div | Mod
  | And | Or | Equals | If
  | FuncApp | Null
  deriving (Show, Eq)

-- The operator name defined and the list of inner expressions. That means
-- that all operators have at least 2 operands, inner list is empty. If inner list
-- is inhabited then the operator has more than 2 operands. This type defines an binary operator that acctually is a n-tuple operator.
type Operator = (OperatorName, [Expr])

data Associativity = LeftAssoc | RightAssoc
  deriving (Show, Eq)

instance Ord Associativity where
  compare LeftAssoc RightAssoc = LT
  compare RightAssoc LeftAssoc = GT
  compare a b                  = EQ

data BinaryOperatorDef = BinOpDef
  { getOp    :: OperatorName,
    getStr   :: String,
    getPrec  :: Int,
    getAssoc :: Associativity
  }
  deriving (Show, Eq)

instance Ord BinaryOperatorDef where
  compare a b = 
    let
      pa = getPrec a
      pb = getPrec b
      aa = getAssoc a
      ab = getAssoc b
    in if pa == pb then compare aa ab else compare pa pb 

builtInBinaryOperators :: [BinaryOperatorDef]
builtInBinaryOperators = [
  BinOpDef Sum    "+"  10 LeftAssoc,
  BinOpDef Sub    "-"  10 LeftAssoc,
  BinOpDef And    "&&" 07 LeftAssoc,
  BinOpDef Or     "||" 06 LeftAssoc,
  BinOpDef Equals "==" 08 LeftAssoc,
  BinOpDef Mod    "%"  15 LeftAssoc,
  BinOpDef Mult   "*"  15 LeftAssoc,
  BinOpDef Div    "/"  15 LeftAssoc]

funcAppBinaryOpDef = BinOpDef FuncApp "." 99 RightAssoc

-- Group all binaryoperators in groups that contain the same precedence and associativity
-- ordered by precedence than associativity
groupedBinaryOps :: [[BinaryOperatorDef]]
groupedBinaryOps = 
  let
    sorted = sortBy (flip compare) builtInBinaryOperators
    grouped = groupBy (\a b -> (a >= b) && (a <= b)) sorted
  in grouped

binaryOperatorP :: [BinaryOperatorDef] -> Parser Char ParseError Operator
binaryOperatorP binOpsD = choice $ map (\opD -> (\_ -> (getOp opD, [])) <$> (stringWs $ getStr opD)) binOpsD

binaryOperatorsTableP :: Parser Char ParseError Expr -> [[BinaryOperatorDef]] -> Parser Char ParseError Expr
binaryOperatorsTableP closed binOps = foldl foldF closed binOps
  where
    getAssocBinaryP x
      | getAssoc x == LeftAssoc  = leftBinOpExpr
      | getAssoc x == RightAssoc = rightBinOpExpr

    foldF hc x@(hx:_) = (getAssocBinaryP hx) hc (binaryOperatorP x)
-- ======================= Binary Operators ========================

-- ============== UTILS ====================
ws :: Eq e => Parser Char e ()
ws = const () <$> many (satisfy isSpace)

betWs :: Parser Char ParseError a -> Parser Char ParseError a
betWs p = betweenC ws p ws

stringWs :: String -> Parser Char ParseError String
stringWs = betWs . string

parentsC :: Parser Char ParseError a -> Parser Char ParseError a
parentsC p = betweenC (betWs $ char '(') p (betWs $ char ')')

-- toBuiltInOp :: String -> Expr
-- toBuiltInOp a
--   | isNothing f =  error $ "Expected operator, but got " ++ show a 
--   | otherwise   = Op $ fromMaybe Null $ do bop <- f; return (getOp bop)
--   where
--     f = find (\bop -> str bop == a) builtInBinaryOperators
-- ============== UTILS ====================

-- ============== EXPR PARSERS ====================
finalP :: Parser Char ParseError [Expr]
finalP = sepEndBy exprP (betWs $ char ';')

exprP :: Parser Char ParseError Expr
exprP = lambdaP <|> ifThenElseP <|> closedP

lambdaP :: Parser Char ParseError Expr
lambdaP = LambdaAbstraction <$> (string "lamb" *> ws *> identifierP <* ws <* string "=>" <* ws) <*> exprP

ifThenElseP :: Parser Char ParseError Expr
ifThenElseP = rightBinOpExpr term ((\x -> (If, [x])) <$> (stringWs "?" *> exprP <* stringWs ":"))

term :: Parser Char ParseError Expr
term = binaryOperatorsTableP funcAppP groupedBinaryOps

funcAppP :: Parser Char ParseError Expr
funcAppP = leftBinOpExpr1 funcAppP' (binaryOperatorP [funcAppBinaryOpDef]) <|> closedP

funcAppP' :: Parser Char ParseError Expr
funcAppP' = closedP <|> builtInOpP

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
builtInOpP :: Parser Char ParseError Expr
builtInOpP = (Op . fst) <$> binaryOperatorP builtInBinaryOperators
