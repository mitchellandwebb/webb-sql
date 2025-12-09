module Webb.Sql.ActiveRecord.ActiveRecord where

import Prelude

import Control.Monad.State (State, execState)
import Data.Array as A
import Webb.State.Prelude (mmodify_)


{- The active record maps fields on the db table to fields on a public type. It also exposes particular queries. -}


type ActiveRecord = 
  { name :: String
  , table :: String
  , fields :: Array { public :: String, private :: String }
  , queries :: Array { public :: String, query :: Query }
  }
  
data Query 
  -- Query, and select only the single item, if it exists
  = SelectOne { query :: String, recordType :: String }

  -- Query, and select all of the results
  | SelectMany { query :: String, recordType :: String }
  
  -- The single item that this record refers to
  | RelOne { recordType :: String, foreignKey :: Array String }
  
  -- The many items that refer to _this_ item
  | RelMany { recordType :: String, foreignKey :: Array String }
  
type ActiveRecordM' a = State ActiveRecord a
type ActiveRecordM = ActiveRecordM' Unit

activeRecord :: String -> String -> ActiveRecordM -> ActiveRecord
activeRecord name table prog = let 
  rec = execState prog { name, table, fields: [], queries: [] }
  in rec
  
addField :: String -> String -> ActiveRecordM
addField public private = do
  mmodify_ $ \s -> s { fields = A.snoc s.fields { public, private }}

addQuery :: String -> Query -> ActiveRecordM
addQuery public query = do
  mmodify_ $ \s -> s { queries = A.snoc s.queries { public, query }}
  
-- Say that a local field needs to try to _become_ the record type via the
-- foreign key.
local :: String -> Array String -> Query
local recordType key = RelOne { recordType, foreignKey: key } 

-- Say that the local field needs to _become_ an array of the foreign record type,
-- populated by matching records from the underlying table to the current record
-- type.
many :: String -> Array String -> Query
many recordType key = RelMany { recordType, foreignKey: key } 

-- Choose the first one matching the query.
queryOne :: String -> String -> Query
queryOne recordType select = SelectOne { recordType, query: select }

-- Choose the first one matching the query.
queryMany :: String -> String -> Query
queryMany recordType select = SelectMany { recordType, query: select }
  
class SetField a where
  setField :: String -> a -> ActiveRecordM
  
instance SetField String where
  setField = addField
  
instance SetField Query where
  setField = addQuery
  
infix 5 setField as := 


