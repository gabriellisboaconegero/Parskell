{-# LANGUAGE MultilineStrings #-}
import Parser
import Control.Applicative
import Data.Char

a = """
lamb a =>
  lamb b =>
    a + b;
lamb f =>
  (f + -1) + 5;

lamb f =>
  lamb a =>
  f. (g. a + 5)
"""
type Identifier = String

type ParseError = Int

data Expr = Var Identifier
  | LiteralInteger Int
  | FunctionApp Identifier Expr
  | Sum Expr Expr
  deriving (Show, Eq)

instance Treeable Expr String where
  node lhs "+" rhs = Sum lhs rhs
  node (Var lhs) "." rhs = FunctionApp lhs rhs
  node lhs op rhs = error $ show (lhs, op, rhs)

data Term = Value Expr
  | LambdaAbstraction Identifier Term
  deriving (Show, Eq)

-- ============== UTILS ====================
ws :: Eq e => Parser Char e ()
ws = const () <$> many (satisfy isSpace)

betWs :: Parser Char ParseError a -> Parser Char ParseError a
betWs p = betweenC ws p ws

parentsC :: Parser Char ParseError a -> Parser Char ParseError a
parentsC p = betweenC (betWs $ char '(') p (betWs $ char ')')
-- ============== UTILS ====================

-- ============== EXPR PARSERS ====================
-- expr ::= sumTerm | funcAppP | closed
-- sumTerm ::= sumTerm' (' '* '+' ' '* sumTerm')+
-- symTerm' ::= funcAppP | closed
-- funcApp ::= ID (' '* '.' ' '* closed)+
-- closed ::= ID | INT | '(' expr ')'
exprP :: Parser Char ParseError Expr
exprP = sumTerm <|> funcAppP <|> closedP

sumTerm :: Parser Char Int Expr
sumTerm = leftBinOpExpr1 sumTerm' (betWs $ string "+")

sumTerm' :: Parser Char Int Expr
sumTerm' = funcAppP <|> closedP

funcAppP :: Parser Char Int Expr
funcAppP = rightBinOpExpr1 closedP (betWs $ string ".")

closedP :: Parser Char ParseError Expr
closedP = (LiteralInteger <$> literalIntegerP) <|> (Var <$> identifierP) <|> parentsC exprP
-- ============== EXPR PARSERS ====================

-- ============== TERM PARSERS ====================
finalP :: Parser Char ParseError [Term]
finalP = sepBy termP (betWs $ char ';')

termP :: Parser Char ParseError Term
termP = lambdaP <|> (Value <$> exprP)

-- LAMBDA ::= ' '* 'amb' ' '* ID ' '* '=>' ' '* Term
lambdaP :: Parser Char ParseError Term
lambdaP = LambdaAbstraction <$> (string "lamb" *> ws *> identifierP <* ws <* string "=>" <* ws) <*> termP
-- ============== TERM PARSERS ====================

-- ============== GENERAL PARSERS ====================
-- ID ::= ALPHA(ALPHANUM | '_')*
identifierP :: Parser Char ParseError Identifier
identifierP = satisfy isAlpha <:> many (satisfy isAlphaNum <|> char '_')

-- INT ::= [-]DIGIT+
literalIntegerP :: Parser Char ParseError Int
literalIntegerP = read <$> integer
