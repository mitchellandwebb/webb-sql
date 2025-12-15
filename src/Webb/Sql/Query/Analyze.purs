module Webb.Sql.Query.Analyze where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String as Str
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Class (class MonadEffect)
import Webb.Monad.Prelude (forceMaybe', notM)
import Webb.Sql.Query.Parser (From(..), TableData)
import Webb.Sql.Query.Parser as P
import Webb.Sql.Query.Token (Token)
import Webb.State.Prelude (aread, awrite)
import Webb.Stateful (localEffect)
import Webb.Stateful.MapColl (MapColl, newMap)
import Webb.Stateful.MapColl as M
import Webb.Validate.Validate (Validate, assert, runValidate_)


{- Analyze the query tree. Build the symbols table, verify all symbols are known, 
  type-check all expressions, check for illegal expressions in particular clauses,
  obtain a type for the 'This' type while checking for ambiguous types, and then
  generate a final TableDef type for the query so that the query's behavior as a 
  Table is actually working. Also publish the 'This' type.
-}
