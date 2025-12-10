module Webb.Sql.Query.Token where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String as Str
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (sequence)
import Parsing (Position(..))
import Parsing as P
import Parsing.Combinators (try)
import Parsing.Combinators.Array as PC
import Parsing.String (string)
import Parsing.String.Basic as PB

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
  | COMMA
  | DOT
  | STAR
  | SINGLE_Q
  | DOUBLE_Q
  | LEFT_PAREN
  | RIGHT_PAREN
  | LIKE
  | GT
  | GTE
  | LT
  | LTE
  | EQUAL
  | ON
  | IDENT
  
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
  
isIdentifier :: Token -> Boolean
isIdentifier (parse) = case parse.kind of
  SELECT -> true
  FROM -> true
  WHERE -> true
  ON -> true
  IDENT -> true
  GROUP -> true
  ORDER -> true
  BY -> true
  INNER -> true
  OUTER -> true
  JOIN -> true
  LEFT -> true
  RIGHT -> true
  LIMIT -> true
  THIS -> false -- this can't be an identifier. It's a special keyword.
  COMMA -> false
  STAR -> false
  DOT -> false
  SINGLE_Q -> false
  DOUBLE_Q -> false
  LEFT_PAREN -> false
  RIGHT_PAREN -> false
  LIKE -> true
  GT -> false
  GTE -> false
  LT -> false
  LTE -> false
  EQUAL -> false
    
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
    singleQ <|>
    doubleQ <|>
    leftParen <|>
    rightParen <|>
    like <|>
    limit <|>
    gt <|>
    gte <|>
    lt <|>
    lte <|>
    equal <|>
    on <|>
    ident
    
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
    try (string lower) <|> try (string upper)

-- The select token. It allows for any case variation of the word "select"
select :: Parser Token
select = forToken SELECT do anyCase "select"

from :: Parser Token
from = forToken FROM do anyCase "from"

where' :: Parser Token
where' = forToken WHERE do anyCase "where"

order :: Parser Token
order = forToken ORDER do anyCase "order"

group :: Parser Token
group = forToken GROUP do anyCase "group"

by :: Parser Token
by = forToken BY do anyCase "by"

inner :: Parser Token
inner = forToken INNER do anyCase "inner"

join :: Parser Token
join = forToken JOIN do anyCase "join"

outer :: Parser Token
outer = forToken OUTER do anyCase "outer"

left :: Parser Token
left = forToken LEFT do anyCase "left"

right :: Parser Token
right = forToken RIGHT do anyCase "right"

this :: Parser Token
this = forToken THIS do anyCase "this"

limit :: Parser Token
limit = forToken LIMIT do anyCase "limit"

dot :: Parser Token
dot = forToken DOT do anyCase "."

star :: Parser Token
star = forToken STAR do anyCase "*"

comma :: Parser Token
comma = forToken COMMA do anyCase ","

singleQ :: Parser Token
singleQ = forToken SINGLE_Q do anyCase "'"

doubleQ :: Parser Token
doubleQ = forToken DOUBLE_Q do anyCase "\""

leftParen :: Parser Token
leftParen = forToken LEFT_PAREN do anyCase "("

rightParen :: Parser Token
rightParen = forToken RIGHT_PAREN do anyCase ")"

like :: Parser Token
like = forToken LIKE do anyCase "like"

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

on :: Parser Token
on = forToken ON do anyCase "on"

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
    char <- try alphaNum <|> try (string "_")
    pure char
    
  alphaNum = try do
    char <- PB.alphaNum
    pure $ fromCharArray [char]
    
