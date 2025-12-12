module Webb.Sql.Analyze.Types where

import Prelude

import Data.Map (Map)
import Data.Maybe (Maybe)
import Effect.Class (class MonadEffect)
import Webb.Sql.Query.Parser as P

type SelectTree = P.Query

data ValueType 
  = Integer
  | Real
  | Text
  | Bool
  | Field
  
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

type TableDefLookup = 
  { ex :: External_
  , locals :: LocalTables
  , aliases :: AliasLookup
  }
  
class External a where
  tableDef :: a -> String -> Maybe TableDef
  tableExists :: a -> String -> Boolean
  warn :: forall m. MonadEffect m => a -> String -> m Unit
  hasErrors :: forall m. MonadEffect m => a -> m Boolean

newtype External_ = External__ (forall r. (forall z. External z => z -> r) -> r)

wrap :: forall z. External z => z -> External_
wrap z = External__ (_ $ z)

instance External (External_) where 
  tableDef (External__ run) = run tableDef
  tableExists (External__ run) = run tableExists
  warn (External__ run) = run warn
  hasErrors (External__ run) = run hasErrors
