module Webb.Sql.Analyze.Contexts where

import Prelude
import Webb.Sql.Analyze.Types

import Control.Monad.State (StateT)


{- Define the various contexts that represent how knowledge changes -}

type QueryContext r = 
  { ex :: External_
  , tree :: SelectTree
  | r }
  
type AliasContext r = 
  QueryContext ( aliases :: AliasLookup | r)
  
type LocalContext r = 
  AliasContext ( locals :: LocalTables | r )
  
type TableContext r = 
  LocalContext ( tables :: TableDefLookup | r )
  
-- When all we know is the query
type QueryM r = StateT (QueryContext r) 

-- When we know about aliases
type AliasM r = StateT (AliasContext r) 

-- When we know about local tables
type LocalM r = StateT (LocalContext r) 

-- When we know about all tables
type TableM r = StateT (TableContext r) 
