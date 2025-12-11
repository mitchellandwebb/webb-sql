module Webb.Sql.Query.Parser.Token where

import Prelude

import Control.Alt ((<|>))
import Data.Identity (Identity)
import Data.List (List)
import Parsing (ParserT, Position(..), fail)
import Parsing.Combinators (try)
import Parsing.Token as T
import Webb.Sql.Query.Token (Token, TokenType(..))

{- Basic parsers for each individual string token. -}

type Parser a = ParserT (List Token) Identity a

-- Gets the next token. After that, we have to specify that each token is valid
-- by comparing to the type.
next :: Parser Token
next = T.token getPos
  where
  getPos :: Token -> Position
  getPos (tok) = 
    Position { column: tok.column, index: tok.index, line: tok.line }
    
hasType :: TokenType -> Parser Token
hasType kind = try do
  token <- next
  if token.kind == kind then
    pure token
  else 
    fail $ "Expected token type" <> show kind
    
select :: Parser Token
select = hasType SELECT

from :: Parser Token
from = hasType FROM

where' :: Parser Token
where' = hasType WHERE

order :: Parser Token
order = hasType ORDER

group :: Parser Token
group = hasType GROUP

by :: Parser Token
by = hasType BY

inner :: Parser Token
inner = hasType INNER

join :: Parser Token
join = hasType JOIN

left :: Parser Token
left = hasType LEFT

right :: Parser Token
right = hasType RIGHT

outer :: Parser Token
outer = hasType OUTER

this :: Parser Token
this = hasType THIS

leftp :: Parser Token
leftp = hasType LEFT_PAREN

rightp :: Parser Token
rightp = hasType RIGHT_PAREN

dot :: Parser Token
dot = hasType DOT

comma :: Parser Token
comma = hasType COMMA

star :: Parser Token
star = hasType STAR

like :: Parser Token
like = hasType LIKE

limit :: Parser Token
limit = hasType LIMIT

asc :: Parser Token
asc = hasType ASC

desc :: Parser Token
desc = hasType DESC

andOp :: Parser Token
andOp = hasType AND_OP

orOp :: Parser Token
orOp = hasType OR_OP

plus :: Parser Token
plus = hasType PLUS

minus :: Parser Token
minus = hasType MINUS

slash :: Parser Token
slash = hasType SLASH

mod :: Parser Token
mod = hasType MOD

gt :: Parser Token
gt = hasType GT

gte :: Parser Token
gte = hasType GTE

lt :: Parser Token
lt = hasType LT

lte :: Parser Token
lte = hasType LTE

equal :: Parser Token
equal = hasType EQUAL

notEqual :: Parser Token
notEqual = hasType NOT_EQUAL

on :: Parser Token
on = hasType ON

stringLit :: Parser Token
stringLit = hasType STRING

numberLit :: Parser Token
numberLit = hasType NUMBER

booleanLit :: Parser Token
booleanLit = hasType BOOLEAN

ident :: Parser Token
ident = hasType IDENT