module Webb.Sql.Query.Parser where

import Prelude

import Control.Alt ((<|>))
import Data.Array as A
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String as Str
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple.Nested ((/\))
import Parsing (fail)
import Parsing as P
import Parsing.Combinators (option, optionMaybe, sepBy1, try)
import Parsing.Combinators.Array as PC
import Parsing.String as PS
import Parsing.String.Basic as PB
import Webb.Sql.Query.Parser.Token as T
import Webb.Sql.Query.Token (Token, TokenType(..))


{- We are parsing a SELECT sql statement. Thus, there are many clauses
  that we need to address, that involve discarding tokens. But the key part
  is defining the data structure of the tree; plenty of tokens are incidental
  and only used to make parts of the data recognizable.
-}

-- A column, as in SELECT *, table.*, age, name, table.name as nickname FROM

type Parser a = T.Parser a
type StringParser a = P.Parser String a

data Query

type Select =
  { columns :: Array Column
  }

type Column = { expr :: ValueExpr, alias :: Maybe String }

type ColumnName = { table :: Maybe Token, field :: Token }

data ValueExpr
  = Field ColumnName
  | Call { name :: Token, args :: Array ValueExpr }
  | Prim Literal
  | Wildcard
  
type Where = { join :: Join }
  
data Join 
  = Table TableData
  | Tables (Array TableData)
  | JoinOp { kind :: JoinType, table1 :: Join, table2 :: Join, on :: ValueExpr }
  | SubQuery Query
  
type TableData = { table :: Token, alias :: Maybe Token }
  
data JoinType = InnerJoin | OuterJoin | LeftJoin | RightJoin
  
type IntegerLit = 
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
  = Integer IntegerLit
  | Real NumberLit
  | Text StringLit
  | Bool BooleanLit
  
type OrderBy = 
  { fields :: Array ColumnName
  , asc :: Boolean
  }

type GroupBy = 
  { fields :: Array ColumnName
  }

type Limit = 
  { value :: IntegerLit
  }
  
valueExpr :: Parser ValueExpr
valueExpr = do
  fail "No value expression parser defined"
  
-- Joins. Note the dummy argument to enable recursion.
join :: Unit -> Parser Join
join _ = try do
  try joinOp <|> try tables <|> try table <|> try subquery
  where
  tableData = try do
    t <- T.ident
    alias <- optionMaybe T.ident
    pure $ { table: t, alias }
    
  table = try do
    td <- tableData
    pure $ Table td
    
  tables = try do
    ts <- sepBy1 tableData T.comma        
    pure $ Tables (A.fromFoldable ts)
    
  joinOp = try do
    t1 <- join unit
    kind <- joinType
    t2 <- join unit
    on <- onClause
    pure $ JoinOp { kind, table1: t1, table2: t2, on }
    
  joinType = try do
    innerJoin <|> leftJoin <|> rightJoin <|> outerJoin
    
    where
    innerJoin = try do 
      void $ (T.inner *> T.join) <|> try (T.join)
      pure InnerJoin

    leftJoin = try do 
      void $ (T.left *> T.join )
      pure LeftJoin

    rightJoin = try do
      void (T.right *> T.join )
      pure RightJoin

    outerJoin = try do 
      void (T.outer *> T.join )
      pure OuterJoin
      
  onClause = try do
    void $ T.on
    valueExpr  
    
  subquery = try do
    fail "No subquery implementation for 'join'"
  
-- Attempt to parse a string. Fail if the string parse fails
parseString :: forall a. String -> StringParser a -> Parser a
parseString s prog = try do  
  let e = P.runParser s prog
  case e of
    Left (P.ParseError msg _) -> do fail msg
    Right res -> pure res

-- Parse the token into an in-memory boolean value.
booleanLit :: Parser BooleanLit
booleanLit = try do
  token <- T.booleanLit
  let str = Str.toLower token.string
  if str == "true" then 
    pure { token, value: true }
  else if str == "false" then
    pure { token, value: false }
  else 
    fail "Not a valid boolean literal"
    
-- Parse the token into an in-memory integer value.
integerLit :: Parser IntegerLit
integerLit = try do 
  token <- T.numberLit
  num <- parseString token.string PB.intDecimal
  pure { token, value: num }
  
-- Turn the token into a FP number representation in memory.
numberLit :: Parser NumberLit
numberLit = try do 
  token <- T.numberLit
  num <- parseString token.string PB.number
  pure { token, value: num }
  
-- Turn the string literal into an in-memory string, for analysis (if needed).
-- This is particularly useful if the code-generated string does not look the
-- same as the actual text of the string literal.
stringLit :: Parser StringLit
stringLit = try do
  token <- T.stringLit
  value <- parseString token.string contents
  pure { token, value }
  
  where
  -- Read visible characters from the string, and add to the output. Thus,
  -- newlines and tab characters don't count as true characters unless they
  -- were explicitly included in the string.
  contents :: StringParser String
  contents = try do 
    let delim = try (PS.string "\"") <|> try (PS.string "'")
    _ <- delim
    strings <- PC.many $ try escaped <|> try visible <|> (pure "")
    _ <- delim
    pure $ Str.joinWith "" strings
    
    where
    escaped = try do
      _ <- PS.string "\\"
      char <- PB.lower
      let str = fromCharArray [char]
          map = Map.fromFoldable
            [ "n" /\ "\n"
            , "t" /\ "\t"
            , "r" /\ "\r"
            ]
      case Map.lookup str map of
        Nothing -> do
          fail "Escaped character is not found"
        Just res -> do
          pure res
        
    visible = try do 
      char <- PS.anyChar
      let str = fromCharArray [char]
          invisible = Set.fromFoldable [ "\n", "\r", "\t" ]
      if Set.member str invisible then do
        fail "Not a visible character"
      else do
        pure str

groupBy :: Parser GroupBy  
groupBy = try do
  _ <- T.group
  _ <- T.by
  names <- columnNames
  pure $ { fields: names }
      
orderBy :: Parser OrderBy  
orderBy = try do
  _ <- T.order
  _ <- T.by
  names <- columnNames
  asc <- option true ascending
  pure $ { fields: names, asc }
  
  where 
  ascending = do
    token <- try T.asc <|> try T.desc
    case token.kind of
      ASC -> pure true
      DESC -> pure false
      _ -> fail "Not a recognized direction"
      
limit :: Parser Limit
limit = try do
  _ <- T.limit
  int <- integerLit
  pure $ { value: int }
  
columnName :: Parser ColumnName
columnName = try do
  table <- optionMaybe do 
    table <- T.ident
    _ <- T.dot
    pure table
  field <- T.ident
  pure { table, field }
  
columnNames :: Parser (Array ColumnName)
columnNames = try do 
  list <- sepBy1 columnName T.comma
  pure $ A.fromFoldable list

  
