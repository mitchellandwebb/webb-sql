module Webb.Sql.Analyze.SymbolChecks where

import Prelude
import Webb.Sql.Analyze.AnalyzeM
import Webb.Sql.Analyze.Types

import Control.Alt ((<|>))
import Data.Array as Arr
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Webb.Monad.Prelude (forceMaybe', notM)
import Webb.Sql.Analyze.Types as T
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
  
confirmSelect :: forall m. MonadEffect m => AnalyzeM m Unit
confirmSelect = do pure unit
  
confirmFrom :: forall m. MonadEffect m => AnalyzeM m Unit
confirmFrom = do pure unit
  
confirmWhere :: forall m. MonadEffect m => AnalyzeM m Unit
confirmWhere = do pure unit
  
confirmGroupBy :: forall m. MonadEffect m => AnalyzeM m Unit
confirmGroupBy = do pure unit
  
confirmOrderBy :: forall m. MonadEffect m => AnalyzeM m Unit
confirmOrderBy = do pure unit
  

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



