module Webb.Sql.Query.Parser where

import Prelude

import Data.Identity (Identity)
import Data.List (List)
import Parsing (ParserT, Position(..))
import Parsing.Token as T
import Webb.Sql.Query.Token (Token)


type Parser a = ParserT (List Token) Identity a

next :: Parser Token
next = T.token getPos
  where
  getPos :: Token -> Position
  getPos (tok) = 
    Position { column: tok.column, index: tok.index, line: tok.line }