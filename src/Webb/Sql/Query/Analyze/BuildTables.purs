module Webb.Sql.Query.Analyze.BuildTables where

import Prelude
import Webb.Sql.Query.Analyze.AnalyzeM
import Webb.Sql.Query.Analyze.Types

import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as Str
import Effect.Class (class MonadEffect)
import Webb.Monad.Prelude (notM)
import Webb.Random (randomId)
import Webb.Sql.Query.Parser (TableData)
import Webb.Sql.Query.Parser as P
import Webb.Sql.Query.Token (Token)
import Webb.State.Prelude (mread)
import Webb.Stateful.MapColl as M


{- Analyze the query tree for aliases and local tables. -}

-- When querying, we can analyze the tree to build lookups from aliases to tables.
-- This is the first part of building symbol tables.
buildAliases :: forall m. MonadEffect m => AnalyzeM m Boolean
buildAliases = do
  addAllTables 
  notM hasErrors
  
  where
  -- Go through the entire 'from' segment and document the tables.
  addAllTables :: AnalyzeM m Unit
  addAllTables = do
    this <- mread
    case this.tree.from of
      P.Tables tables -> do
        for_ tables addTableData
      P.Joins join -> do 
        addJoin join
          
  addJoin :: P.Join -> AnalyzeM m Unit
  addJoin join = do
    case join of
      P.Table td -> do 
        addTableData td
      P.SubQuery _ -> do 
        pure unit
      P.JoinOp { table1, table2 } -> do 
        addJoin table1
        addJoin table2

  -- Add a single table's data to the table lookups. If no alias, use
  -- the table's name directly.
  addTableData :: TableData -> AnalyzeM m Unit 
  addTableData td = do 
    this <- mread
    let table = td.table.string
        malias = _.string <$> td.alias
        name = Str.toLower $ fromMaybe table malias :: String
    confirmAlias name
    M.insert this.aliases name table

confirmAlias :: forall m. MonadEffect m => String -> AnalyzeM m Unit
confirmAlias alias = do
  this <- mread
  whenM (M.member this.aliases alias) do
    warn $ "The table alias is already used: " <> alias

-- Build local tables from knowledge of the aliases.
buildLocalTables :: forall m . MonadEffect m => 
  AnalyzeM m Boolean
buildLocalTables = do
  this <- mread
  case this.tree.from of
    P.Tables _ -> do
      pure $ true -- no local tables
    P.Joins join -> do 
      buildJoin join
      
      notM hasErrors

  where
  buildJoin :: 
    P.Join -> AnalyzeM m Unit
  buildJoin join = do
    case join of
      P.Table _ -> do 
        pure unit
      P.JoinOp { table1, table2 } -> do
        buildJoin table1 
        buildJoin table2 
      P.SubQuery { query, alias } -> do
        buildSubquery query alias 
    
  buildSubquery :: 
    P.Query -> Maybe Token -> AnalyzeM m Unit
  buildSubquery query mtoken = do
    this <- mread
    case mtoken of 
      Nothing -> do 
        -- TODO -- should have lines/columns for ENTIRE query that causes this.
        warn "A subquery must be aliased in a join"
      Just token -> do
        let alias = token.string
        confirmAlias alias
        tableId <- randomId
        M.insert this.aliases alias tableId

        let tableDef = analyzeQuery this.ex query
        M.insert this.locals tableId tableDef