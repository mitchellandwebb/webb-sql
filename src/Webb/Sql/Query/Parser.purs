module Webb.Sql.Query.Parser where

import Prelude

import Data.Identity (Identity)
import Data.List (List)
import Data.Maybe (Maybe)
import Parsing (ParserT, Position(..), fail)
import Parsing.Combinators (try)
import Parsing.Combinators.Array as PC
import Webb.Sql.Query.Parser.Token as T
import Webb.Sql.Query.Token (Token, TokenType(..))


{- We are parsing a SELECT sql statement. Thus, there are many clauses
  that we need to address, that involve discarding tokens. But the key part
  is defining the data structure of the tree; plenty of tokens are incidental
  and only used to make parts of the data recognizable.
-}

-- A column, as in SELECT *, table.*, age, name, table.name as nickname FROM

type Parser a = T.Parser a

type Column = { expr :: ValueExpr, alias :: Maybe String }

data ValueExpr
  = Field { prefix :: Maybe Token, suffix :: Token }
  | Call { name :: Token, args :: Array ValueExpr }
  | Prim Literal
  
type IntLit = 
  { token :: Token
  , value :: Int
  }

type NumberLit = 
  { token :: Token
  , value :: Number
  }

type StringLit = 
  { token :: Token
  , value :: String
  }

type BooleanLit = 
  { token :: Token
  , value :: Boolean
  }
  
data Literal
  = Integer IntLit
  | Real NumberLit
  | Text StringLit
  | Bool BooleanLit
  
type OrderBy = 
  { fields :: Array Token
  , asc :: Boolean
  }

type GroupBy = 
  { fields :: Array Token
  }

type Limit = 
  { token :: Token
  , value :: Int
  }
  
-- TODO -- need tokens for for numbers and Booleans and raw strings -- which
-- must come in as tokens, perhaps? Or else spaces get dropped. Indeed ... raw
-- tokens seem more like distinct important groups. A string is one of those things.

{-}
boolean :: Parser BooleanLit
boolean = try do
  token <- T.ident
-}
