module Webb.Sql.Query.Analyze.IllegalCheck where

import Prelude
import Webb.Sql.Query.Analyze.AnalyzeM
import Webb.Sql.Query.Analyze.Types

import Data.Foldable (for_)
import Data.Set (Set)
import Data.Set as Set
import Data.String as Str
import Effect.Class (class MonadEffect)
import Webb.Sql.Query.Parser as P
import Webb.State.Prelude (mread)


{- Despite being formally okay, there are several usages in clauses that are _illegal_
  for reasons that are highly-specific to the clause. In particular, Aggregation
  functions are illegal when used in ValueExpressions during Select, From, and Where
  These illegal usages, and any others, are specified here so we can notify the user.
-}


illegalCheck :: forall m. MonadEffect m => Analyze m Unit
illegalCheck = do
  checkSelect
  checkFrom
  checkWhere
  checkGroupBy
  checkOrderBy
  
  where
  -- Aggregation is allowed
  checkSelect = do 
    pure unit

  checkFrom = do 
    this <- mread
    case this.tree.from of
      P.Tables _ -> pure unit
      P.Joins join -> case join of
        P.Table _ -> pure unit
        P.SubQuery _ -> pure unit
        P.JoinOp { on } -> do 
          -- Aggregation cannot happen during table specification
          noAggregate on

  checkWhere = do 
    this <- mread
    let expr = this.tree.where.expr
    -- Aggregation cannot happen during filtering
    noAggregate expr

  -- Aggregation is allowed
  checkGroupBy = do 
    this <- mread
    let groupBy = this.tree.groupBy
    for_ groupBy \gb -> do
      for_ gb.fields \_field -> do 
        pure unit

  -- Aggregation is allowed
  checkOrderBy = do 
    this <- mread
    let orderBy = this.tree.orderBy
    for_ orderBy \ob -> do
      for_ ob.fields \_field -> do 
        pure unit


noAggregate :: forall m. MonadEffect m => P.ValueExpr -> Analyze m Unit
noAggregate expr = case expr of
  P.Field _ -> pure unit
  P.This _ -> pure unit
  P.Prim _ -> pure unit
  P.Wildcard _ -> pure unit
  P.Call s -> do
    let name = Str.toLower (s.name.string)
    when (Set.member name aggregates) do
      warn $ message name
  where
  aggregates = Set.fromFoldable
    [ "count", "min", "max", "sum", "avg"
    ]
    
  message name = 
    "Aggregate function '" <> name <> "' cannot be used here"      