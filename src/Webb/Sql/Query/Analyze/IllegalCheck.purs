module Webb.Sql.Query.Analyze.IllegalCheck where

import Prelude
import Webb.Sql.Query.Analyze.AnalyzeM
import Webb.Sql.Query.Analyze.Types

import Data.Newtype (unwrap)
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

checkAllIllegal :: forall m. MonadEffect m => Analyze m Unit
checkAllIllegal = do
  this <- mread 
  let tree = this.tree
  illegal tree.select
  illegal tree.from
  illegal tree.where

class IllegalCheck a where
  illegal :: forall m. MonadEffect m => a -> Analyze m Unit

instance IllegalCheck P.Select where
  illegal sel = do
    let cols = unwrap sel
    illegal <$!> cols
    
instance IllegalCheck P.From where
  illegal from = case from of
    P.Tables _ -> pure unit
    P.Joins join -> illegal join
    
instance IllegalCheck P.Where where
  illegal w = do
    let expr = unwrap w
    illegal expr
    
instance IllegalCheck P.Join where
  illegal join = case join of
    P.Table _ -> pure unit
    P.SubQuery _ -> pure unit
    P.JoinOp s -> do 
      illegal s.on
      illegal s.table1
      illegal s.table2
      
instance IllegalCheck P.Column where
  illegal col = do 
    let s = unwrap col
    illegal s.expr
      
instance IllegalCheck P.ValueExpr where
  illegal expr = case expr of
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
