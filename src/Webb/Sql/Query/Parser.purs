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
import Parsing.Combinators (option, optionMaybe, sepBy, sepBy1, try)
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

type Query = 
  { select :: Select
  , from :: From
  , where :: Where
  , groupBy :: Maybe GroupBy
  , orderBy :: Maybe OrderBy
  , limit :: Maybe Limit
  }

type Select =
  { columns :: Array Column
  }

type Column = { expr :: ValueExpr, alias :: Maybe Token }

type ColumnName = { table :: Maybe Token, field :: Token }

data ValueExpr
  = Field ColumnName
  | This { field :: Token }
  | Call { name :: Token, args :: Array ValueExpr }
  | Prim Literal
  | Wildcard { table :: Maybe Token }
  
data From = Tables (Array TableData) | Joins Join
  
data Join 
  = Table TableData
  | JoinOp { kind :: JoinType, table1 :: Join, table2 :: Join, on :: ValueExpr }
  | SubQuery { query :: Query, alias :: Maybe Token }
  
type TableData = { table :: Token, alias :: Maybe Token }
  
data JoinType = InnerJoin | OuterJoin | LeftJoin | RightJoin

type Where = 
  { expr :: ValueExpr
  }
  
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
  
query :: Parser Query
query = do
  select_ <- select
  from_ <- from
  where_ <- where'
  groupBy_ <- optionMaybe groupBy
  orderBy_ <- optionMaybe orderBy
  limit_ <- optionMaybe limit

  pure 
    { select: select_
    , from: from_
    , where: where_
    , groupBy: groupBy_
    , orderBy: orderBy_
    , limit: limit_
    }
  
select :: Parser Select
select = do 
  columns <- PC.many column
  pure $ { columns }
  
  where
  column :: Parser Column
  column = do 
    expr <- valueExpr
    alias <- optionMaybe T.ident
    pure $ { expr, alias }
  
valueExpr :: Parser ValueExpr
valueExpr = try do
  expr <- try field 
    <|> try primitive 
    <|> try wildcard 
    <|> try (call unit)
    <|> try this
  pure expr
  
  where
  field = Field <$> columnName

  primitive = Prim <$> literal

  wildcard = try bare <|> try withTable
    where
    bare :: Parser ValueExpr
    bare = do 
      _ <- T.star
      pure $ Wildcard { table: Nothing }
      
    withTable :: Parser ValueExpr
    withTable = do
      table <- T.ident
      _ <- T.dot
      _ <- T.star
      pure $ Wildcard { table: Just table }

  call _ = do 
    name <- T.ident
    _ <- T.leftp
    args <- A.fromFoldable <$> sepBy valueExpr T.comma
    _ <- T.rightp
    pure $ Call { name, args }
    
  this = do 
    _ <- T.this
    _ <- T.dot
    field' <- T.ident
    pure $ This { field: field' }
    
from :: Parser From
from = do
  try tables <|> try (joins unit)
  where
  tables :: Parser From
  tables = try do
    ts <- sepBy1 tableData T.comma        
    pure $ Tables $ A.fromFoldable ts
    
  joins :: Unit -> Parser From
  joins _ = try do Joins <$> join

tableData :: Parser TableData
tableData = try do
  t <- T.ident
  alias <- optionMaybe T.ident
  pure $ { table: t, alias }
  
join :: Parser Join
join = try do
  try (joinOp unit) <|> try table <|> try (subquery unit)
  where
    
  table = try do
    td <- tableData
    pure $ Table td
    
  -- dummy argument to enable recursion.
  joinOp _ = try do
    t1 <- join 
    kind <- joinType
    t2 <- join 
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
    
  -- Recursion dummy argument.
  subquery _ = try do 
    query' <- query
    alias <- optionMaybe $ T.ident
    pure $ SubQuery { query: query', alias }
    
where' :: Parser Where
where' = do 
  expr <- valueExpr
  pure $ { expr }
  
-- Attempt to parse a string. Fail if the string parse fails
parseString :: forall a. String -> StringParser a -> Parser a
parseString s prog = try do  
  let e = P.runParser s prog
  case e of
    Left (P.ParseError msg _) -> do fail msg
    Right res -> pure res
    
literal :: Parser Literal
literal = do
  try int<|> try bool <|> try string <|> try number
  
  where
  int = Integer <$> integerLit
  bool = Bool <$> booleanLit
  string = Text <$> stringLit
  number = Real <$> numberLit
  

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

  
