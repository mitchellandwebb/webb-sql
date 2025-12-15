module Webb.Sql.Query.Analyze.TypeCheck where

import Prelude
import Webb.Sql.Query.Analyze.AnalyzeM
import Webb.Sql.Query.Analyze.Types

import Control.Alt ((<|>))
import Data.Array as Arr
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Webb.Monad.Prelude (throwString)
import Webb.Sql.Query.Parser (ValueExpr)
import Webb.Sql.Query.Parser as P
import Webb.Sql.Query.Token (Token)
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


checkAllTypes :: forall m. MonadEffect m => AnalyzeM m Boolean
checkAllTypes = do
  this <- mread
  let tree = this.tree
  checkTypes tree.select
  checkTypes tree.from
  checkTypes tree.where
  checkTypes <$!> tree.groupBy
  checkTypes <$!> tree.orderBy
  isSuccess  

-- Data types that can be type-checked
class CheckTypes a where
  checkTypes :: forall m. MonadEffect m => a -> Analyze m Unit
  
-- Data types that actually have a concrete type.
class GetType a where
  getType :: forall m. MonadEffect m => a -> Analyze m ValueType

instance CheckTypes P.Select where
  checkTypes this = do
    let cols = unwrap this
    for_ cols checkTypes
    
instance CheckTypes P.From where
  checkTypes from = case from of
    P.Tables _ -> pure unit
    P.Joins join -> checkTypes join

instance CheckTypes P.Where where
  checkTypes w = do
    let expr = unwrap w
    checkTypes expr
    
instance CheckTypes P.GroupBy where
  checkTypes w = do
    let exprs = unwrap w
    checkTypes <$!> exprs
    
instance CheckTypes P.OrderBy where
  checkTypes w = do
    let { fields } = unwrap w
    checkTypes <$!> fields
    
instance CheckTypes P.Join where
  checkTypes join = case join of
    P.Table _ -> pure unit
    P.SubQuery _ -> pure unit
    P.JoinOp s -> do
      checkTypes s.on
      checkTypes s.table1
      checkTypes s.table2
    
instance CheckTypes P.Column where
  checkTypes this = do 
    let { expr } = unwrap this
    checkTypes expr
    
instance CheckTypes P.ValueExpr where
  checkTypes this = case this of
    P.Field _ -> pure unit
    P.This _ -> pure unit
    P.Wildcard _ -> pure unit 
    P.Prim _ -> pure unit
    P.Call { name, args } -> do confirmFn name args

instance GetType P.ValueExpr where
  getType expr = case expr of
    P.Field col -> getType col
    P.This _ -> pure Any
    P.Wildcard _ -> pure Never
    P.Prim lit -> getType lit
    P.Call { name } -> do getFnType name
    
instance GetType P.ColumnName where
  getType col = do
    let s = unwrap col
    getFieldType s.table s.field
    
instance GetType P.Literal where
  getType lit = pure case lit of
    P.Integer _ -> Integer
    P.Real _ -> Real
    P.Text _ -> Text
    P.Bool _ -> Bool

confirmFn :: forall m. MonadEffect m => Token -> Array ValueExpr -> Analyze m Unit
confirmFn nameT args = do
  this <- mread
  let name = nameT.string
  def <- force ("No such function: " <> name) $ fnDef this.ex name
  case def.args of
    Finite params expected -> do
      let actual = Arr.length args
      if actual /= expected then do 
        warn $ "Wrong argument count. Expected " <> 
          show expected <> ", but got " <> show actual
      else do 
        let pairs = Arr.zip params args
        for_ pairs (uncurry confirmArg)
    Infinite expected -> do
      let pairs = args <#> \arg -> expected /\ arg
      for_ pairs (uncurry confirmArg)

confirmArg :: forall m. MonadEffect m => 
  ValueType -> P.ValueExpr -> Analyze m Unit
confirmArg expected expr = do 
  actual <- getType expr
  unless (expected <=: actual) do
    warn "Type doesn't match"

getFnType :: forall m. MonadEffect m => Token -> Analyze m ValueType
getFnType nameT = do
  this <- mread
  let name = nameT.string
  def <- force ("No such function: " <> name) $ fnDef this.ex name
  pure $ def.return
  
getFieldType :: forall m. MonadEffect m => 
  Maybe Token -> Token -> Analyze m ValueType
getFieldType mtableT fieldT = do
  this <- mread
  case mtableT of
    Nothing -> do 
      names <- M.values this.aliases
      if Arr.length names == 1 then do
        liftEffect do throwString $ "Ambiguous field: " <> fieldT.string
      else do 
        name <- force ("No tables found") (Arr.head names)
        let field = fieldT.string
        lookupFieldType name field

    Just tableT -> do
      let alias = tableT.string
          field = fieldT.string
      mname <- M.lookup this.aliases alias
      name <- force ("Alias does not exist: " <> alias) mname
      lookupFieldType name field

  where
  lookupFieldType name field = do
    this <- mread
    mlocal <- M.lookup this.locals name
    let mglobal = tableDef this.ex name
    def <- force ("Table not found: " <> name) (mlocal <|> mglobal)
    
    let mfieldDef = Map.lookup field def.fields
    fieldDef <- force ("Field not found: " <> field) mfieldDef
    pure fieldDef.kind
        