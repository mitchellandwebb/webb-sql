module Webb.Sql.Analyze.Alias where

import Prelude
import Webb.Sql.Analyze.Contexts
import Webb.Sql.Analyze.Types

import Control.Monad.Trans.Class (lift)
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Class (class MonadEffect)
import Webb.Sql.Analyze.Query (hasFailed, validate)
import Webb.Sql.Query.Parser (TableData)
import Webb.Sql.Query.Parser as P
import Webb.Sql.Query.Token (Token)
import Webb.State.Prelude (amodify_, aread, awrite, mmodify_, mread)
import Webb.Stateful.MapColl (MapColl, newMap)
import Webb.Stateful.MapColl as M
import Webb.Validate.Validate (Validate, assert)



{- Analysis for when we know about aliases. -}

-- It's one thing for the compiler to verify you did things right. It's
-- another thing for the compiler to only tell you that things are wrong, in
-- an unparsable way that leaves the mind with an empty queue.


-- Build local tables from knowledge of the aliases.
buildLocalTables :: forall r m . MonadEffect m => 
  LocalM r m (Maybe LocalTables)
buildLocalTables = do
  this <- mread
  case this.tree.from of
    P.Tables _ -> do
      pure $ Just Map.empty
    P.Joins join -> do 
      lookups <- newMap
      awrite this.aliases lookups

      locals <- newMap
      validate do 
        buildJoin join lookups locals

      ifM hasFailed (do
        pure Nothing 
      ) (do 
        lookups' <- aread lookups
        mmodify_ $ _ { aliases = lookups'}

        locals' <- aread locals
        pure $ Just locals'
      )
  where
  buildJoin :: 
    P.Join -> MapColl String String -> MapColl String TableDef -> Validate m Unit
  buildJoin join lookups locals = do
    case join of
      P.Table _ -> do 
        pure unit
      P.JoinOp { table1, table2 } -> do
        buildJoin table1 lookups locals
        buildJoin table2 lookups locals
      P.SubQuery { query, alias } -> do
        buildSubquery query alias lookups locals
    
  buildSubquery :: 
    P.Query -> Maybe Token -> 
    MapColl String String -> MapColl String TableDef -> 
    Validate m Unit
  buildSubquery query malias lookups locals = do
    case malias of 
      Nothing -> do 
        -- TODO -- for parsing warnings, errors typically will also 
        -- go with the Start/End token that is responsible for the
        -- error. In this case, we'd like to publish that the entire subquery
        -- itself causes the error -- but if not that, perhaps each
        -- element of the tree needs to be annotated with the start/end position
        -- for the element? That allows it to be independent of the structure
        -- itself. In other words, we need a tree of Nodes for this to work,
        -- which is annoying (of course)
        assert false "A subquery must be aliased in a join"
      Just alias -> do
        -- TODO -- add query to local table AND to alias
        assert false "How to build a subquery definition is not yet defined"