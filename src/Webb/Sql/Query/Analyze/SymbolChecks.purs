module Webb.Sql.Query.Analyze.SymbolChecks where

import Prelude
import Webb.Sql.Query.Analyze.AnalyzeM
import Webb.Sql.Query.Analyze.Types

import Control.Alt ((<|>))
import Data.Array as Arr
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Class (class MonadEffect, liftEffect)
import Webb.Monad.Prelude (forceMaybe')
import Webb.Sql.Query.Parser as P
import Webb.Sql.Query.Token (Token)
import Webb.State.Prelude (mread)
import Webb.Stateful.MapColl as M

{- Analyze the query for symbols, and check that all symbols exist -- 
  both fields and tables. 
-}


checkAllSymbols :: forall m. MonadEffect m =>
  AnalyzeM m Boolean
checkAllSymbols = do
  this <- mread
  let tree = this.tree
  checkSymbols tree.select
  checkSymbols tree.from
  checkSymbols tree.where
  checkSymbols <$!> tree.groupBy
  checkSymbols <$!> tree.orderBy
  isSuccess

class CheckSymbols a where
  checkSymbols :: forall m. MonadEffect m => a -> Analyze m Unit
  
instance CheckSymbols P.Select where
  checkSymbols sel = do
    let cols = unwrap sel
    for_ cols checkSymbols

instance CheckSymbols P.From where
  checkSymbols from = case from of 
    P.Tables _ -> do pure unit
    P.Joins join -> do
      checkSymbols join
      
instance CheckSymbols P.Where where
  checkSymbols where' = do 
    let w = unwrap where'
    checkSymbols w
    
instance CheckSymbols P.GroupBy where
  checkSymbols gb = do
    let fields = unwrap gb
    for_ fields checkSymbols
    
instance CheckSymbols P.OrderBy where
  checkSymbols this = do
    let s = unwrap this
    for_ s.fields checkSymbols
      
instance CheckSymbols P.Join where
  checkSymbols join = case join of
    P.Table _ -> do pure unit
    P.SubQuery _ -> do pure unit
    P.JoinOp s -> do 
      checkSymbols s.on
      checkSymbols s.table1
      checkSymbols s.table2
    
instance CheckSymbols P.Column where
  checkSymbols col = do
    let s = unwrap col
    checkSymbols s.expr
    
instance CheckSymbols P.ValueExpr where
  checkSymbols expr = case expr of 
    P.Field name -> do 
      checkSymbols name
          
    P.Wildcard s -> do
      let mtable = s.table
      confirmAlias mtable
      
    P.Call s -> do
      confirmFn s.name
      for_ s.args checkSymbols      
        
    -- The rest contain no symbols
    P.This _ -> do pure unit
    P.Prim _ -> do pure unit
    
instance CheckSymbols P.ColumnName where
  checkSymbols col = do
    let { table, field } = unwrap col
    confirmAlias table 
    confirmField table field
  
confirmFn :: forall m. MonadEffect m =>
  Token -> AnalyzeM m Unit
confirmFn tok = do
  let name = tok.string
  this <- mread
  unless (fnExists this.ex name) do
    warn $ "Function does not exist: " <> name

-- Warn if the alias doesn't exist.
confirmAlias :: forall m. MonadEffect m =>
  Maybe Token -> AnalyzeM m Unit
confirmAlias mtok = do
  for_ mtok confirmAlias'

confirmAlias' :: forall m. MonadEffect m => 
  Token -> AnalyzeM m (Maybe TableDef)
confirmAlias' tok = do
  let alias = tok.string
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
          
confirmField :: forall m. MonadEffect m =>
  Maybe Token -> Token -> Analyze m Unit
confirmField mtable field = do
  case mtable of
    Nothing -> confirmSoloField field
    Just table -> confirmAliasedField table field

-- Confirm that the aliased field exists.
confirmAliasedField :: forall m. MonadEffect m => 
  Token -> Token -> AnalyzeM m Unit
confirmAliasedField aliasT fieldT = do
  let alias = aliasT.string
      field = fieldT.string
  mtableDef <- confirmAlias' aliasT
  case mtableDef of
    Nothing -> do 
      pure unit
    Just tableDef -> do
      unless (Map.member field tableDef.fields) do
        warn $ "Field '" <> alias <> "." <> field <> "' was not found"
        
-- Confirm that the solo field can be matched to a table, and that it belongs on
-- that table. For this to work, there must be a _single_ table defined.
confirmSoloField :: forall m. MonadEffect m =>
  Token -> AnalyzeM m Unit
confirmSoloField fieldT = do
  let field = fieldT.string
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


