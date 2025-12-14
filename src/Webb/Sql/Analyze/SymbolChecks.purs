module Webb.Sql.Analyze.SymbolChecks where

import Prelude
import Webb.Sql.Analyze.AnalyzeM
import Webb.Sql.Analyze.Types

import Control.Alt ((<|>))
import Data.Array as Arr
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Webb.Monad.Prelude (forceMaybe', notM)
import Webb.Sql.Query.Parser as P
import Webb.State.Prelude (mread)
import Webb.Stateful.MapColl as M

{- Analyze the query for symbols, and check that all symbols exist -- 
  both fields and tables. 
-}


confirmSymbols :: forall m. MonadEffect m =>
  AnalyzeM m Boolean
confirmSymbols = do
  confirmSelect
  confirmFrom
  confirmWhere
  confirmGroupBy
  confirmOrderBy

  notM hasErrors
  
-- Confirm the symbols in all the columns.
confirmSelect :: forall m. MonadEffect m => AnalyzeM m Unit
confirmSelect = do 
  this <- mread
  for_ this.tree.select.columns confirmColumn
  
  where
  confirmColumn col = do confirmExpr col.expr
  
confirmExpr :: forall m. MonadEffect m => P.ValueExpr -> AnalyzeM m Unit
confirmExpr expr = case expr of
  P.Field name -> do 
    confirmColumnName name
        
  P.Wildcard s -> do
    let mtable = _.string <$> s.table
    for_ mtable confirmAlias
    
  P.Call s -> do
    -- If  it is a function call, confirm the function and then the rest
    -- of the expression.
    let name = _.string s.name
    confirmFn name
    for_ s.args confirmExpr      
      
  -- Type inference for 'this' has to be done separately.
  P.This _ -> do pure unit

  -- Primitives have no symbols.
  P.Prim _ -> do pure unit

-- Confirm the join expressions.
confirmFrom :: forall m. MonadEffect m => AnalyzeM m Unit
confirmFrom = do 
  this <- mread
  case this.tree.from of
    P.Tables _ -> do 
      -- Tables have no unknown symbols
      pure unit
    
    P.Joins join -> do
      confirmJoin join
      
  where
  confirmJoin join = case join of
    P.Table _ -> do 
      -- No symbols exist in the table itself
      pure unit
    P.JoinOp s -> do 
      confirmExpr s.on
      confirmJoin s.table1
      confirmJoin s.table2
    P.SubQuery _ -> do 
      -- Subqueries were confirmed earlier during table definition generation.
      pure unit
  
confirmWhere :: forall m. MonadEffect m => AnalyzeM m Unit
confirmWhere = do 
  this <- mread
  confirmExpr this.tree.where.expr
  
confirmGroupBy :: forall m. MonadEffect m => AnalyzeM m Unit
confirmGroupBy = do 
  this <- mread
  for_ this.tree.groupBy \{ fields } -> do
    confirmColumnNames fields
  
confirmOrderBy :: forall m. MonadEffect m => AnalyzeM m Unit
confirmOrderBy = do
  this <- mread
  for_ this.tree.orderBy \{ fields } -> do
    confirmColumnNames fields
    
confirmColumnNames :: forall m. MonadEffect m => 
  Array P.ColumnName -> AnalyzeM m Unit
confirmColumnNames columns = do
  for_ columns confirmColumnName

confirmColumnName :: forall m. MonadEffect m => 
  P.ColumnName -> AnalyzeM m Unit
confirmColumnName column = do
  let field = _.string column.field
      mtable = _.string <$> column.table
  case mtable of
    Nothing -> do
      confirmSoloField field
    Just table -> do
      confirmField table field
  
confirmFn :: forall m. MonadEffect m =>
  String -> AnalyzeM m Unit
confirmFn name = do
  this <- mread
  unless (fnExists this.ex name) do
    warn $ "Function does not exist: " <> name

-- Warn if the alias doesn't exist.
confirmAlias :: forall m. MonadEffect m =>
  String -> AnalyzeM m Unit
confirmAlias alias = do
  void $ confirmAlias' alias

confirmAlias' :: forall m. MonadEffect m => 
  String -> AnalyzeM m (Maybe TableDef)
confirmAlias' alias = do
  this <- mread  
  mtable <- M.lookup this.aliases alias
  case mtable of 
    Nothing -> do
      warn $ "Alias '" <> alias <> "' does not refer to a known table"
      pure Nothing
    Just table -> do
      mlocal <- M.lookup this.locals table
      let mglobal = tableDef this.ex table
          mtableDef = mlocal <|> mglobal
          
      case mtableDef of
        Nothing -> do
          warn $ "Alias '" <> alias <> "' refers to undefined table '" <> table
          pure Nothing
        Just tableDef -> do
          pure $ Just tableDef

-- Confirm that the aliased field exists.
confirmField :: forall m. MonadEffect m => 
  String -> String -> AnalyzeM m Unit
confirmField alias field = do
  mtableDef <- confirmAlias' alias
  case mtableDef of
    Nothing -> do 
      pure unit
    Just tableDef -> do
      unless (Map.member field tableDef.fields) do
        warn $ "Field '" <> alias <> "." <> field <> "' was not found"
        
-- Confirm that the solo field can be matched to a table, and that it belongs on
-- that table. For this to work, there must be a _single_ table defined.
confirmSoloField :: forall m. MonadEffect m =>
  String -> AnalyzeM m Unit
confirmSoloField field = do
  this <- mread
  len <- M.length this.aliases
  if len /= 1 then do
    warn $ "Field '" <> field <> 
      "' is ambiguous. The query should use exactly one table, or "
      <> "the field should be identified with dot-syntax"
  else do 
    tableIds <- M.values this.aliases
    tableId <- liftEffect do forceMaybe' "No tables found" (Arr.head tableIds)
    mlocal <- M.lookup this.locals tableId
    let mglobal = tableDef this.ex tableId
        mtableDef = mlocal <|> mglobal
    case mtableDef of
      Nothing -> do
        warn $ "No table definition found for field '" <> field <> "'"
      Just (tableDef :: TableDef) -> do
        unless (Map.member field tableDef.fields) do 
          warn $ "Field '" <> field <> "' could not be found on table '" 
            <> tableDef.name <> "'"



