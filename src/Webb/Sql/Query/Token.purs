module Webb.Sql.Query.Token where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class as Err
import Data.Array as A
import Data.Either (Either(..), isRight)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String as Str
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (class Foldable, sequence)
import Effect.Class (class MonadEffect, liftEffect)
import Parsing (ParseError(..), Position(..), fail, runParser)
import Parsing as P
import Parsing.Combinators (lookAhead, option, try)
import Parsing.Combinators.Array as PC
import Parsing.String as PS
import Parsing.String.Basic as PB
import Webb.Monad.Prelude (throwString)

{- Since SQL does not care about case, we tokenize to eliminate string differences, and to make clear the actual tokens we are working with.
-}

data TokenType
  = SELECT 
  | FROM
  | WHERE
  | ORDER
  | GROUP
  | BY
  | INNER
  | JOIN
  | OUTER
  | LEFT
  | RIGHT
  | THIS
  | LIMIT
  | ASC
  | DESC
  | COMMA
  | STRING
  | NUMBER
  | BOOLEAN
  | DOT
  | STAR
  | LEFT_PAREN
  | RIGHT_PAREN
  | LIKE
  | GT
  | GTE
  | LT
  | LTE
  | EQUAL
  | NOT_EQUAL
  | ON
  | IDENT
  | AND_OP
  | OR_OP
  | PLUS
  | MINUS
  | SLASH
  | MOD
  
derive instance Eq TokenType
derive instance Ord TokenType
derive instance Generic TokenType _
instance Show TokenType where show = genericShow
  
type Token =
  { index :: Int
  , line :: Int
  , column :: Int
  , columnEnd:: Int
  , string :: String
  , kind :: TokenType
  }
  
-- Parse small strings into tokens.
type Parser = P.Parser String

forToken :: TokenType -> Parser String -> Parser Token
forToken kind prog = try do
  old <- getPosition
  str <- prog
  new <- getPosition
  pure $ 
    { line: old.line
    , column: old.column
    , columnEnd: new.column
    , string: str
    , kind
    , index: old.index
    }
  
  where 
  getPosition = do
    (Position s) <- P.position
    pure s
    
tokenize :: forall m. MonadEffect m => String -> m (Array Token)
tokenize s = case runParser s tokens of
  Left (ParseError msg (Position pos)) -> liftEffect do 
    throwString $ "Error at " <> show pos <> ": " <> msg
  Right r -> pure r
  
parse :: forall m a. MonadEffect m => String -> Parser a -> m a
parse str prog = case runParser str prog of
  Left (ParseError msg (Position pos)) -> liftEffect do 
    throwString $ "Error at " <> show pos <> ": " <> msg
  Right r -> pure r
    
-- Turn a string into an array of tokens. We ignore
-- whitespace intentionally.
tokens :: Parser (Array Token)
tokens = do
  arr <- PC.many do 
    t <- token
    PB.skipSpaces
    pure t
  pure arr

-- Obtain a single token.
token :: Parser (Token)
token = try do 
  select <|> 
    from <|>
    where' <|>
    order <|>
    group <|>
    by <|>
    inner <|>
    join <|>
    outer <|>
    left <|>
    right <|>
    this <|>
    dot <|>
    comma <|>
    star <|>
    andOp <|>
    orOp <|>
    plus <|>
    minus <|>
    slash <|>
    mod <|>
    leftParen <|>
    rightParen <|>
    like <|>
    limit <|>

    gte <|>
    lte <|>
    gt <|>
    lt <|>

    notEqual <|>
    equal <|>

    on <|>
    asc <|>
    desc <|>

    stringLit <|>
    numberLit <|>
    boolean <|>

    ident -- ALWAYS last, to prevent conflict with all keywords
    
-- Parse the string, but try _all_ the possible case variations.
anyCase :: String -> Parser String
anyCase str = try do
  let lowers = Str.split (Str.Pattern "") str
      parsers = lowers <#> lowerOrUpper
  chars <- sequence parsers
  pure $ Str.joinWith "" chars
  where 
  lowerOrUpper char = try do
    let upper = Str.toUpper char
        lower = Str.toLower char
    try (PS.string lower) <|> try (PS.string upper)

-- The select token. It allows for any case variation of the word "select"
select :: Parser Token
select = forToken SELECT do notIdent $ anyCase "select"

from :: Parser Token
from = forToken FROM do notIdent $ anyCase "from"

where' :: Parser Token
where' = forToken WHERE do notIdent $ anyCase "where"

order :: Parser Token
order = forToken ORDER do notIdent $ anyCase "order"

group :: Parser Token
group = forToken GROUP do notIdent $ anyCase "group"

by :: Parser Token
by = forToken BY do notIdent $ anyCase "by"

inner :: Parser Token
inner = forToken INNER do notIdent $ anyCase "inner"

join :: Parser Token
join = forToken JOIN do notIdent $ anyCase "join"

outer :: Parser Token
outer = forToken OUTER do notIdent $ anyCase "outer"

left :: Parser Token
left = forToken LEFT do notIdent $ anyCase "left"

right :: Parser Token
right = forToken RIGHT do notIdent $ anyCase "right"

this :: Parser Token
this = forToken THIS do notIdent $ anyCase "this"

limit :: Parser Token
limit = forToken LIMIT do notIdent $ anyCase "limit"

asc :: Parser Token
asc = forToken ASC do notIdent $ anyCase "asc"

desc :: Parser Token
desc = forToken DESC do notIdent $ anyCase "desc"

dot :: Parser Token
dot = forToken DOT do anyCase "."

star :: Parser Token
star = forToken STAR do anyCase "*"

comma :: Parser Token
comma = forToken COMMA do anyCase ","

andOp :: Parser Token
andOp = forToken AND_OP do anyCase "&&"

orOp :: Parser Token
orOp = forToken OR_OP do anyCase "||"

plus :: Parser Token
plus = forToken PLUS do anyCase "+"

minus :: Parser Token
minus = forToken MINUS do anyCase "-"

slash :: Parser Token
slash = forToken SLASH do anyCase "/"

mod :: Parser Token
mod = forToken MOD do anyCase "%"

leftParen :: Parser Token
leftParen = forToken LEFT_PAREN do anyCase "("

rightParen :: Parser Token
rightParen = forToken RIGHT_PAREN do anyCase ")"

-- SELECT 'abc' FROM...
-- SELECT "abc" FROM...
stringLit :: Parser Token
stringLit = forToken STRING do
  try doubleQ <|> try singleQ
  where
  doubleQ = try do
    let delim = PS.string "\""
    s1 <- delim
    str <- contentsWithout "\"" 
    s2 <- delim
    pure $ Str.joinWith "" [s1, str, s2]
  
  singleQ = try do
    let delim = PS.string "'"
    s1 <- delim
    str <- contentsWithout "'"
    s2 <- delim
    pure $ Str.joinWith "" [s1, str, s2]
    
  contentsWithout str = try do
    strings <- PC.many (reject str stringChar)
    pure $ Str.joinWith "" strings
    
  stringChar = try do
    try escaped <|> try (reject "\"" anyChar)
    
  escaped = try do
    start <- PS.string "\\"
    char <- anyChar
    pure $ Str.joinWith "" [ start, char ]
    
  anyChar = try do
    char <- PS.anyChar
    pure $ fromCharArray [char]
    
-- 1.0, +1, -1.0, 1.0e1, 1E-1.0, 0.3, 00.3
numberLit :: Parser Token
numberLit = forToken NUMBER do
  n1 <- option "" sign
  n2 <- number
  n3 <- option "" exponent
  pure $ Str.joinWith "" [n1, n2, n3]
  
  where
  sign = try (PS.string "+") <|> try (PS.string "-")
  
  number = try do
    first <- fromChars <$> PC.many1 PB.digit
    dec <- option "" do 
      dot' <- PS.string "."
      rest <- fromChars <$> PC.many1 PB.digit
      pure $ dot' <> rest
    pure $ first <> dec
  
  exponent = try do 
    e <- (anyCase "e")
    s <- number
    pure $ Str.joinWith "" [e, s]
    
fromChars :: forall f. Foldable f => f Char -> String
fromChars chars = fromCharArray $ A.fromFoldable chars
    
reject :: forall a. Show a => Eq a => a -> Parser a -> Parser a
reject a prog = try do
  res <- prog
  if res == a then do
    fail $ "Rejected parse: " <> show a
  else do
    pure res

like :: Parser Token
like = forToken LIKE do notIdent $ anyCase "like"

gt :: Parser Token
gt = forToken GT do anyCase ">"

gte :: Parser Token
gte = forToken GTE do anyCase ">="

lt :: Parser Token
lt = forToken LT do anyCase "<"

lte :: Parser Token
lte = forToken LTE do anyCase "<="

equal :: Parser Token
equal = forToken EQUAL do anyCase "="

notEqual :: Parser Token
notEqual = forToken NOT_EQUAL do anyCase "!="

on :: Parser Token
on = forToken ON do notIdent $ anyCase "on"

boolean :: Parser Token
boolean = forToken BOOLEAN do notIdent $ anyCase "true" <|> anyCase "false"

-- Executes the parser. Fails it if the parser is followed by 
-- an extension as an identifier.
notIdent :: Parser String -> Parser String
notIdent prog = try do
  t <- prog

  -- If followed by a letter, num, or underscore, it's an ident. 
  whenM (succeeds letterNumUnderscore) do
    fail "Token is really an identifier"

  pure t
  where
  letterNumUnderscore = try do
    char <- try alphaNum <|> try (PS.string "_")
    pure char
    
  alphaNum = try do
    char <- PB.alphaNum
    pure $ fromCharArray [char]
    
-- Attempts the given parser, and returns whether it _would_ have suceeded.
succeeds :: forall a. Parser a -> Parser Boolean
succeeds prog = do
  e <- Err.try do lookAhead prog
  pure $ isRight e

-- Identifier starts with a letter, then can be letters, numbers, or
-- underscores
ident :: Parser Token
ident = forToken IDENT do
  first <- letter
  rest <- PC.many letterNumUnderscore
  let string = Str.joinWith "" $ [first] <> rest
  pure string
  
  where
  letter :: Parser String
  letter = try do
    char <- PB.letter
    pure $ fromCharArray [char]
    
  letterNumUnderscore = try do
    char <- try alphaNum <|> try (PS.string "_")
    pure char
    
  alphaNum = try do
    char <- PB.alphaNum
    pure $ fromCharArray [char]
    
