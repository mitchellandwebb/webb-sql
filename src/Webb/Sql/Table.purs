module Webb.Sql.Table where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.State (State, execState)
import Data.Array as A
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Webb.Sql.Field as F
import Webb.Sql.Validate (Validate, fatal)
import Webb.State.Prelude (mmodify_)


{- Defines the table data structure and language for building a table. -}


type Table = 
  { name :: String
  , fields :: Array Field
  , primaryKeys :: Array PrimaryKey
  , foreignKeys :: Array ForeignKey
  }
  
type Field = 
  { name :: String
  , attrs :: Array F.Attr
  }
  
type PrimaryKey = Array String

type ForeignKey = 
  { fields :: Array String
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
  
field :: String -> F.FieldM -> TableM
field name prog = do
  let f = F.field name prog
  mmodify_ $ \s -> s { fields = A.snoc s.fields f }
  
primaryKey :: Array String -> TableM
primaryKey names = do
  mmodify_ $ \s -> s { primaryKeys = A.snoc s.primaryKeys names }
  
-- foreignKey ["age", "name"] "other" ["firstAge", "secondAge"]
foreignKey :: Array String -> String -> Array String -> TableM
foreignKey fields table' otherFields = do
  let key = { fields, ref: { table: table', fields: otherFields }} :: ForeignKey
  mmodify_ $ \s -> s { foreignKeys = A.snoc s.foreignKeys key }
  
  
{- When we write a table, we have to perform validation of the table definition itself,
  internally, to verify that it is a well-defined table with primary key, and that its
  foreign keys have valid references, and that its fields have a required type and any
  other important values. But we also have to check the Table's foreign keys against
  _other_ tables. That means we need to inject that external environment, or else we 
  can't perform the check.
-}

-- During validation, we seek to do a couple tasks that aren't
-- particularly obvious, even after we've built the tables. We want to
-- specify that primary key exists only once, that default value matches
-- type; that primary keys can't be used on nullable values, that foreign
-- keys don't overlap and refer to existing values, and so on. What's odd
-- is that it's hard to put the data in a form that makes it easy to
-- say these things. With untyped objects it would be easier -- We just make
-- assumptions and write things. But we can't do that here.

type Definition = 
  { primaryKey :: Array String
  , foreignKeys :: Array ForeignKey
  , fields :: Array Field
  }

{-}
validate :: Table -> External_ -> Validate Definition
validate t ex = do
  when (not $ tableExists ex t.name) do
    fatal $ "Table already exists: " <> t.name
    
  primaryKey <- getPrimaryKey t ex
  foreignKeys <- getForeignKeys t ex
  fields <- getFields t ex
  
  pure { primaryKey, foreignKeys, fields }
  fatal "hello"
  
getPrimaryKey :: Table -> External_ -> Validate (Array String)
getPrimaryKey t ex = do
  let reg = R.fieldRegistry t.fields
  
  let mcomposite = A.head t.primaryKeys
      mfield = A.head fieldPrimaryKeys
      mkey = mcomposite <|> mfield

  case mkey of
    Nothing -> do
      fatal "No primary key found"
    Just key -> do
      pure key 
      
  where
  fieldPrimaryKeys = F.primaryKeys t.fields

-}
  

{-}
checkCompleteFields :: Table -> Validate Unit
checkCompleteFields t = do
  for_ t.fields checkField
  where
  checkField = do
    pure unit

checkPrimaryKey :: Table -> Validate Unit
checkPrimaryKey t = do
  checkExists
  checkOnlyOne
  where
  checkExists = A.find (F.)


-- Foreign keys are defined once for any set, composite or not. Multiple definitions are
-- flagged.
checkForeignKeys :: Table -> External_ -> Validate Unit
checkForeignKeys t ex = do
  checkDistinct
  checkValid
-}

class External a where
  columnExists :: a -> String -> String -> Boolean
  tableExists :: a -> String -> Boolean

newtype External_ = External__ (forall r. (forall z. External z => z -> r) -> r)

wrap :: forall z. External z => z -> External_
wrap z = External__ (_ $ z)

instance External (External_) where 
  columnExists (External__ run) = run columnExists
  tableExists (External__ run) = run tableExists
