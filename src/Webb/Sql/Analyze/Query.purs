module Webb.Sql.Analyze.Query where

import Prelude
import Webb.Sql.Analyze.Contexts
import Webb.Sql.Analyze.Types

import Control.Monad.Trans.Class (lift)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as Str
import Effect.Class (class MonadEffect)
import Webb.Sql.Query.Parser (TableData)
import Webb.Sql.Query.Parser as P
import Webb.State.Prelude (aread, mread)
import Webb.Stateful.MapColl (MapColl, newMap)
import Webb.Stateful.MapColl as M
import Webb.Validate.Validate (Validate, assert, runValidate_)


{- Analysis functions when we only know the query tree. -}

-- When querying, we can always run validation functions.
validate :: forall r m. MonadEffect m => 
  Validate m Unit -> QueryM r m Unit
validate prog = do 
  self <- mread
  runValidate_ (warn self.ex) prog # lift

-- When querying, we can always check if we've failed.
hasFailed :: forall r m. MonadEffect m => QueryM r m Boolean
hasFailed = do 
  self <- mread
  hasErrors self.ex # lift
  
-- When querying, we can analyze the tree to build lookups from aliases to tables.
-- This is the first part of building symbol tables.
buildAliases :: forall r m. MonadEffect m => QueryM r m (Maybe AliasLookup)
buildAliases = do
  self <- mread
  coll <- newMap
  validate do
    addAllTables coll self.tree

  ifM hasFailed (do
    pure Nothing
  ) (do 
    Just <$> aread coll 
  )
  
  where
  -- Go through the entire 'from' segment and document the tables.
  addAllTables :: MapColl String String -> SelectTree -> Validate m Unit
  addAllTables coll tree = do
    case tree.from of
      P.Tables tables -> do
        for_ tables (addTableData coll)
      P.Joins join -> do 
        addJoin coll join
          
  addJoin :: MapColl String String -> P.Join -> Validate m Unit
  addJoin coll join = do
    case join of
      P.Table td -> do 
        (addTableData coll td)
      P.SubQuery _ -> do 
        pure unit
      P.JoinOp { table1, table2 } -> do 
        addJoin coll table1
        addJoin coll table2

  -- Add a single table's data to the table lookups.
  addTableData :: MapColl String String -> TableData -> Validate m Unit 
  addTableData coll td = do 
    let table = td.table.string
        malias = _.string <$> td.alias
        name = Str.toLower $ fromMaybe table malias
    ifM (M.member coll name) (do 
      assert false $ "Table alias is already used: " <> name
    ) (do 
      M.insert coll name table
    )
