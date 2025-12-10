module Webb.Sql.Query.Parser where

import Prelude
import Webb.Sql.Query.Parser.Basic

import Data.Identity (Identity)
import Data.List (List)
import Data.Maybe (Maybe)
import Parsing (ParserT, Position(..), fail)
import Parsing.Combinators (try)
import Parsing.Token as T
import Webb.Sql.Query.Token (Token, TokenType(..))


{- We are parsing a SELECT sql statement. Thus, there are many clauses
  that we need to address, that involve discarding tokens. But the key part
  is defining the data structure of the tree; plenty of tokens are incidental
  and only used to make parts of the data recognizable.
-}

-- A column, as in SELECT *, table.*, age, name, table.name as nickname FROM

type Column = { expr :: ValueExpr, alias :: Maybe String }

data ValueExpr
  = Field { prefix :: Maybe Token, suffix :: Token }
  | Call { name :: Token, args :: Array ValueExpr }
  | Prim Literal
  
type Literal = 
  { token :: Token
  , value :: LiteralValue
  }
  
data LiteralValue 
  = Integer Int
  | Real Number
  | Text String
  | Bool Boolean




