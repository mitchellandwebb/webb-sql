module Webb.Sql.Table.Table where

import Prelude

import Control.Monad.State (State, execState)
import Data.Array as A
import Webb.Sql.Table.Field as F
import Webb.State.Prelude (mmodify_)

{- Defines the table data structure and language for building a table. -}


type Table = 
  { name :: String
  , fields :: Array Field
  , primaryKeys :: Array PrimaryKey
  , foreignKeys :: Array ForeignKey
  }
  
type Field = F.Field
  
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
  
  