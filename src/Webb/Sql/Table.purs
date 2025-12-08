module Webb.Sql.Table where

import Prelude


{- Defines the table data structure and language for building a table. -}


type Table = 
  { name :: String
  , fields :: Array Field
  , primaryKey :: PrimaryKey
  , foreignKeys :: Array ForeignKey
  }
  
type Field = 
  { name :: String
  , attrs :: Array Attr
  }
  
type PrimaryKey = Array String

type ForeignKey = 
  { field :: String
  , ref :: { table :: String, field :: String }
  }
  
data Attr = Nil | FieldType FieldType

data FieldType = Integer
  | Text
  | Real
  | Bool
  
empty :: Table 
empty = 
  { name: ""
  , fields: []
  , primaryKey: []
  , foreignKeys: []
  }
  
type TableM' a = State Table a
type TableM = TableM' Unit