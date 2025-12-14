module Webb.Sql.Query.Analyze.Types where

import Prelude

import Data.Map (Map)
import Data.Maybe (Maybe)
import Webb.Sql.Query.Parser as P

{- Define the base monad for use during analysis, and common effects shared 
  across all modules.
-}

type SelectTree = P.Query

data ValueType 
  = Integer
  | Real
  | Text
  | Bool
  | Nil
  | Field
  | Union ValueType ValueType
  | Product ValueType ValueType
  | Never
  | Any
  
type RecordType = Map String ValueType

-- Look up tables according to their assigned aliases. 
type AliasLookup = Map String String

-- The names of local tables, mapped to their defined fields and field types.
type LocalTables = Map String TableDef

type TableDef = 
  { name :: String
  , fields :: Map String FieldDef
  , primaryKey :: Array String
  , foreignKeys :: Array ForeignKey
  }
  
type FieldDef = 
  { name :: String
  , kind :: ValueType
  , nil :: Boolean
  }

type ForeignKey = 
  { fields :: Array String
  , ref :: { table :: String, fields :: Array String}
  }
  
type FnDef = 
  { name :: String
  , args :: Array ValueType
  , return :: ValueType
  }

class External a where
  tableDef :: a -> String -> Maybe TableDef
  tableExists :: a -> String -> Boolean
  fnDef :: a -> String -> Maybe FnDef
  fnExists :: a -> String -> Boolean
  analyzeQuery :: a -> SelectTree -> TableDef

newtype External_ = External__ (forall r. (forall z. External z => z -> r) -> r)

wrap :: forall z. External z => z -> External_
wrap z = External__ (_ $ z)

instance External (External_) where 
  tableDef (External__ run) = run tableDef
  tableExists (External__ run) = run tableExists
  fnDef (External__ run) = run fnDef
  fnExists (External__ run) = run fnExists
  analyzeQuery (External__ run) = run analyzeQuery
