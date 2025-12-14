module Webb.Sql.Query.Analyze.TypeCheck where

import Prelude
import Webb.Sql.Query.Analyze.AnalyzeM
import Webb.Sql.Query.Analyze.Types

import Control.Alt ((<|>))
import Data.Array as Arr
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (uncurry1, uncurry2)
import Effect.Class (class MonadEffect, liftEffect)
import Webb.Monad.Prelude (forceMaybe')
import Webb.Sql.Query.Parser (ValueExpr)
import Webb.Sql.Query.Parser as P
import Webb.State.Prelude (mread)
import Webb.Stateful.MapColl as M



{- Validate the type usage in each clause of the SQL query. Publish a 
  warning when invalid types are used in particular contexts.

  However, note that if we want to typecheck particular expressions, we have to
  perform particular transformations. This may occur due to a GroupBy, for example.
  HOWEVER -- even with a group-by, the aggregate functions are still available, and so
  are the individual fields -- it's just that now, fields are only for display _within_
  a field -- so this doesn't prevent type-checking; it only prevents the publishing of
  columns. Thus, expressions like MAX(a.age / a.weight) are useful, and allowed.
-}



checkExpr :: forall m. MonadEffect m => ValueExpr -> AnalyzeM m Unit
checkExpr expr = void $ checkExpr' expr

-- Check an expression. Publish errors when they are found.
checkExpr' :: forall m. MonadEffect m => ValueExpr -> AnalyzeM m ValueType
checkExpr' expr = case expr of
  P.Field f -> do
    let field = _.string f.field
        table = _.string <$> f.table
    typeofField table field
    
  P.Prim lit -> do 
    typeofLiteral lit
    
  P.Wildcard _ -> do
    -- A wildcard should NEVER be used ... except in certain cases.
    pure Never
    
  P.Call s -> do 
    -- If we are parsing a call, the value is the return value of the call.
    this <- mread
    let name = s.name.string
    def <- force ("No such function: " <> name) $ fnDef this.ex name
    verifyArgs def s.args
    pure def.return
    
  -- For initial type-checking, an argument off 'this' can have any type.
  -- If everything else type-checks, we'll do inference for the 'this' type
  -- in a separate phase -- if any property has multiple types, we will err.
  P.This _ -> do
    pure Any
    
  where 
  verifyArgs :: FnDef -> Array ValueExpr -> Analyze m Unit
  verifyArgs def args = do
    let expected = Arr.length def.args 
        actual = Arr.length args
    if expected /= actual then do
      warn $ "Function '" <> def.name <> "' needs " <> 
        show expected <> " arguments, but got " <> show actual
    else do
      let pairs = Arr.zip def.args args
      for_ pairs (uncurry verifyType)
      
-- Compare the expression to its expected type. Give a warning if they don't match.
verifyType :: forall m. MonadEffect m => ValueType -> ValueExpr -> Analyze m Unit
verifyType kind expr = do 
  exprKind <- checkExpr' expr
  unless (aIncludesB kind exprKind) do
    warn $ "Expected type '" <> show kind <> "', but got " <> show exprKind
    
typeofLiteral :: forall m. MonadEffect m => P.Literal -> AnalyzeM m ValueType
typeofLiteral lit = pure case lit of
  P.Integer _ -> Integer
  P.Real _ -> Real
  P.Text _ -> Text
  P.Bool _ -> Bool
    
-- Retrieve the type of the field. Assume the field exists and throw if it doesn't.
typeofField :: forall m. MonadEffect m => Maybe String -> String -> AnalyzeM m ValueType
typeofField malias field = do
  tableDef <- getTableDef malias
  fieldDef <- getFieldDef tableDef field
  pure fieldDef.kind
  
getTableDef :: forall m. MonadEffect m => Maybe String -> AnalyzeM m TableDef
getTableDef malias = do
  this <- mread
  case malias of
    Nothing -> do 
      onlyTableDef
    Just alias -> do
      mtable <- M.lookup this.aliases alias
      table <- force ("No such alias: " <> alias) mtable
      getTable table
  where
  getTable :: String -> AnalyzeM m TableDef
  getTable table = do
    this <- mread
    mlocal <- M.lookup this.locals table
    let mglobal = tableDef this.ex table
        mtableDef = mlocal <|> mglobal :: Maybe TableDef
    force ("No definition for table: " <> table) mtableDef
    
onlyTableDef :: forall m. MonadEffect m => AnalyzeM m TableDef
onlyTableDef = do
  this <- mread
  mtable <- Arr.head <$> M.values this.aliases
  table :: String <- force "Too many tables" mtable
  getTable table
  where
  getTable :: String -> AnalyzeM m TableDef
  getTable table = do
    this <- mread
    mlocal <- M.lookup this.locals table
    let mglobal = tableDef this.ex table
        mtableDef = mlocal <|> mglobal :: Maybe TableDef
    force ("No definition for table: " <> table) mtableDef
  
  
-- From a TableDef, get the FieldDef
getFieldDef :: forall m. MonadEffect m => TableDef -> String -> AnalyzeM m FieldDef
getFieldDef tableDef field = do
  getField field
  where
  getField :: String -> AnalyzeM m FieldDef
  getField fi = do
    force ("No such field: " <> fi) (Map.lookup fi tableDef.fields)
  