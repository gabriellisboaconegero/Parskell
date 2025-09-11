module Parser where
import Control.Applicative
import Control.Monad
import Data.List (nub)
import Data.Char
import Data.Maybe

type Offset = Int

data Error i e = Error
  { errOffset :: Offset
  , errType   :: ErrorType i e
  }
  deriving (Eq, Show)

-- The error type in case the parsing fails
data ErrorType i e
  = EndOfInput
  | Unexpected i
  | Expected i i
  | ExpectedOf [i] i
  | ExpectedEndOfInput i
  | CustomError e
  | Empty
  deriving (Eq, Show)

-- The parser type itself.
-- A parser is a function that receives the input (stream)
-- consumes it contents following some rules and return the
-- value parsed and the rest of the input (stream). It can fail
-- so it returns a error or the value and the rest of input
--
-- i : input type (does not need to be a string, so we can parse bits)
-- So the library can be used to parsing file types.
--
-- Example: "6aaaaa aosakas"
-- Parse the number n then proced to parse n letters a
newtype Parser i e a = Parser
  { runParser :: [i] -> Offset -> Either [Error i e] (a, Offset, [i])
  }

parse :: Parser i e a -> [i] -> Either [Error i e] (a, Offset, [i])
parse p = (flip $ runParser p) 0

-- Given a predicate, check if first input symbol
-- satisfy the predicate
token :: (i -> ErrorType i e) -> (i -> Bool) -> Parser i e i
token mkErr predicate = Parser $ \input offset ->
  case input of
    [] -> Left [Error offset EndOfInput]
    hd : rest
      | predicate hd -> Right (hd, offset + 1, rest)
      | otherwise    -> Left [Error offset $ mkErr hd]

satisfy :: (i -> Bool) -> Parser i e i
satisfy = token Unexpected

-- parse the end of input
eof :: Parser i e ()
eof = Parser $ \input offset ->
  case input of
    []     -> Right ((), offset, [])
    hd : _ -> Left [Error offset $ ExpectedEndOfInput hd]

-- verifify if fist input symbol is equal to some symbol x
char :: Eq i => i -> Parser i e i
char x = token (Expected x) (== x)

-- parser a sequence of symbols
string :: Eq i => [i] -> Parser i e [i]
string = traverse char

-- parser one of the symbols
oneOf :: Eq i => [i] -> Parser i e i
oneOf xs = token (ExpectedOf xs) (\x -> any (== x) xs)

-- trys to parse one of the parsers
choice :: Alternative f => [f a] -> f a
choice = asum

-- parser a digit
digit :: Parser Char e Char
digit = oneOf ['0'..'9']

-- parse a natural number. leading left zeros allowed 0001
natural :: Eq e => Parser Char e [Char]
natural = some digit

-- parse the exact number of the other parser
exact :: Alternative f => Int -> f a -> f [a]
exact 0 p = pure []
exact n p = liftA2 (:) p (exact (n-1) p)

-- parse without consuming the string
-- also ignores offset
lookahead :: Parser i e a -> Parser i e a
lookahead (Parser p) = Parser $ \input offset -> do
  (x, _, _) <- p input offset
  return (x, offset, input)

-- combine parser to alternate between them
-- need to end with first parser
sepBy :: Alternative f => f a -> f b -> f [a]
sepBy p sep = liftA2 (:) p (many (sep *> p))

-- combine parser to alternate between them
-- can end with both parsers
sepBy1 :: Alternative f => f a -> f b -> f [a]
sepBy1 p sep = liftA2 (:) p (some (sep *> p))

-- combine parser to alternate between them
-- need to end with first parser, the separator
-- can be at the end
sepEndBy :: Alternative f => f a -> f b -> f [a]
sepEndBy p sep = sepBy p sep <* optional sep

-- combine parser to alternate between them
-- can end with both parsers, the separator
-- can be at the end
sepEndBy1 :: Alternative f => f a -> f b -> f [a]
sepEndBy1 p sep = sepBy1 p sep <* optional sep

-- Sorround functions
surround l r xs= l ++ xs ++ r
s'  = surround "[" "]"
s'' = surround "<" ">"

class Treeable a b where
  node :: a -> b -> a -> a

instance Treeable [Char] [Char] where
  node lhs op rhs = s' (lhs ++ op ++ rhs)

-- Parse a binary operation in right associative order
rightBinOpExpr :: (Eq i, Eq e, Treeable a b) => Parser i e a -> Parser i e b -> Parser i e a
-- E := T <op> E | T
rightBinOpExpr term op = join <$> (term <~> (op <~> (rightBinOpExpr term op <|> term))) <|> term
  where
    join (lhs, (op, rhs)) = node lhs op rhs

-- Parse a binary operation in left associative order
-- E := T (E <op>)*
leftBinOpExpr :: (Eq i, Eq e, Treeable a b) => Parser i e a -> Parser i e b -> Parser i e a
leftBinOpExpr term op =
  let
    fold_ (t, ts) = foldl (\lhs (op, rhs) -> node lhs op rhs) t ts
  in fold_ <$> (term <~> many (op <~> term))

-- many, some, optional defined in Alternative
option :: Alternative f => a -> f a -> f a
option fallback f = fromMaybe fallback <$> optional f

-- =============== Result Manipulators =====================
-- less than <*, <*>, *> so we can still chain but the <* is done first
infixr 3 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a]
p <:> q = liftA2 (:) p q

infixl 2 <++>
(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
p <++> q = liftA2 (++) p q

infixl 3 <~>
(<~>) :: Applicative f => f a -> f b -> f (a, b)
p <~> q = liftA2 (,) p q
-- =============== Result Manipulators =====================

-- instance Functor (Parser i e) where
--   fmap f (Parser p) = Parser $ \input ->
--     case p input of
--       Left err -> Left err
--       Right (output, rest) -> Right (f output, rest)
--
-- using Either as monad and do notation
instance Functor (Parser i e) where
  fmap f (Parser p) = Parser $ \input offset -> do
    (output, offset', rest) <- p input offset
    return (f output, offset', rest)

instance Applicative (Parser i e) where
  pure a = Parser $ \input offset -> Right (a, offset, input)

  Parser f <*> Parser p = Parser $ \input offset -> do
    (f', offset', rest) <- f input offset
    (output, offset'', rest') <- p rest offset'
    return (f' output, offset'', rest')

instance Monad (Parser i e) where
  return = pure

  Parser p >>= k = Parser $ \input offset -> do
    (output, offset', rest) <- p input offset
    runParser (k output) rest offset'

-- OR with backtraking. Be careful, recursion with to the left
(<||>) :: (Eq i, Eq e) => Parser i e a -> Parser i e a -> Parser i e a
Parser l <||> Parser r = Parser $ \input offset ->
  case l input offset of
    Left err ->
      case r input offset of
        Left err' -> Left $ nub $ err <> err'
        Right result2 -> Right result2
    Right result1@(_, off1, _) ->
      case r input offset of
        Left err' -> Right result1
        Right result2@(_, off2, _) -> Right (if off1 > off2 then result1 else result2)

instance (Eq i, Eq e) => Alternative (Parser i e) where
  empty = Parser $ \_ offset -> Left [Error offset Empty]

  Parser l <|> Parser r = Parser $ \input offset ->
    case l input offset of
      Left err ->
        case r input offset of
          Left err' -> Left $ nub $ err <> err'
          Right result2 -> Right result2
      Right result1 -> Right result1