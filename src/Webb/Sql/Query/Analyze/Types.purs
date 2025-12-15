module Webb.Sql.Query.Analyze.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
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
  | Union ValueType ValueType
  | Product ValueType ValueType
  | Never
  | Any
  
-- Is the given type 'b' capable of being used in place of type 'a'?
aIncludesB :: ValueType -> ValueType -> Boolean
aIncludesB a b = case a of
  Integer -> case b of
    Integer -> true
    _ -> false
  Real -> case b of
    Integer -> true
    Real -> true
    _ -> false
  Text -> case b of 
    Text -> true
    _ -> false
  Bool -> case b of
    Bool -> true
    _ -> false
  Nil -> case b of
    Nil -> true
    _ -> false
  Never -> case b of
    Never -> true
    _ -> false
  Union x y -> x <=: b || y <=: b
  Product x y -> x <=: b && y <=: b
  Any -> true
  
infix 10 aIncludesB as <=:
  
instance Eq ValueType where
  eq a b = a <=: b && b <=: a

derive instance Generic ValueType _
instance Show ValueType where
  show (Union a b) = "Union (" <> show a <> ") (" <> show b <> ")"
  show (Product a b) = "Product (" <> show a <> ") (" <> show b <> ")"
  show a = genericShow a
  
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
  , args :: ArgCount
  , return :: ValueType
  }
  
data ArgCount = Infinite ValueType | Finite (Array ValueType) Int

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
