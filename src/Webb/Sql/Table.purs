module Webb.Sql.Table where

import Prelude


{- Defines the table data structure and language for building a table. -}


type Table = 
  { name :: String
  , fields :: Array Field
  , primaryKeys :: Array PrimaryKey
  , foreignKeys :: Array ForeignKey
  }
  
type Field = 
  { name :: String
  , attrs :: Array Attr
  }
  
type PrimaryKey = Array String

type ForeignKey = 
  { fields :: String
  , ref :: { table :: String, fields :: Array String }
  }
  
empty :: Table 
empty = 
  { name: ""
  , fields: []
  , primaryKeys: []
  , foreignKeys: []
  }
  
type TableM' a = State Table a
type TableM = TableM' Unit

table :: String -> TableM -> Table
table name prog = let 
  t = execState prog $ empty { name = name }
  in t
  
field :: String -> FieldM -> TableM
field name prog = do
  let f = F.field name prog
  mmodify_ $ \s -> s { fields = A.snoc s.fields f }
  
primaryKey :: Array String -> TableM
primaryKey names = do
  mmodify_ $ \s -> s { primaryKeys = A.snoc s.primaryKeys names }
  
-- foreignKey ["age", "name"] "other" ["firstAge", "secondAge"]
foreignKey :: Array String -> String -> Array String -> TableM
foreignKey fields table otherFields = do
  let key = { fields, ref: { table, field: otherFields }}
  mmodify_ $ \s -> s { foreignKeys = A.snoc s.foreignKeys key }
  
  
{- When we write a table, we have to perform validation of the table definition itself,
  internally, to verify that it is a well-defined table with primary key, and that its
  foreign keys have valid references, and that its fields have a required type and any
  other important values. But we also have to check the Table's foreign keys against
  _other_ tables. That means we need to inject that external environment, or else we 
  can't perform the check.
-}

-- We use either to short-circuit when the first error is detected during the build. But
-- is that actually what we want? Not really -- for validation, we want to accumulate
-- errors.
validate :: Table -> Validate String
validate t = 

checkCompleteFields :: Table -> Validate String
checkCompleteFields t = 

checkPrimaryKey :: Table -> Validate String
checkPrimaryKey t = do
  checkExists
  checkOnlyOne

-- Foreign keys are defined once for any set, composite or not. Multiple definitions are
-- flagged.
checkForeignKeys :: Table -> External -> Validate String
checkForeignKeys t ex = do
  checkDistinct
  checkValid

class External a where
  columnExists :: a -> String -> String -> Boolean