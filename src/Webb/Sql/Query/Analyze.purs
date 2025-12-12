module Webb.Sql.Query.Analyze where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String as Str
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Class (class MonadEffect)
import Webb.Monad.Prelude (forceMaybe', notM)
import Webb.Sql.Query.Parser (From(..), TableData)
import Webb.Sql.Query.Parser as P
import Webb.Sql.Query.Token (Token)
import Webb.State.Prelude (aread, awrite)
import Webb.Stateful (localEffect)
import Webb.Stateful.MapColl (MapColl, newMap)
import Webb.Stateful.MapColl as M
import Webb.Validate.Validate (Validate, assert, runValidate_)


{- Analyze the query tree.

1. Build symbol lookups for global tables (if any) and local subqueries that
  function as tables.

2. Import table definitions for all tables that are used in this query. Go through
  all clauses and validate that each declared field usage refers to a real field. If
  not, we can end the analysis.
  
3. 
-}

type SelectTree = P.Query

data ValueType 
  = Integer
  | Real
  | Text
  | Bool
  | Field
  
type RecordType = Map String ValueType

-- Look up tables according to their assigned aliases. 
type TableLookup = Map String String

-- The names of local tables, mapped to their defined fields and field types.
type LocalTables = Map String TableDef

type TableDef = 
  { name :: String
  , fields :: Map String FieldDef
  , primaryKey :: Array String
  , foreignKeys :: Array ForeignKey
  }
  
type FieldDef = 
  { name :: String
  , kind :: ValueType
  , nil :: Boolean
  }

type ForeignKey = 
  { fields :: Array String
  , ref :: { table :: String, fields :: Array String}
  }

type TableDefLookup = 
  { ex :: External_
  , locals :: LocalTables
  , aliases :: TableLookup
  }
  
aliasExists :: String -> TableDefLookup -> Boolean
aliasExists alias s = Map.member alias s.aliases
  
lookupTable :: String -> TableDefLookup -> TableDef
lookupTable alias s = localEffect do
  let mdef = (do 
        table :: String <- Map.lookup alias s.aliases 
        tableDef s.ex table <|> Map.lookup table s.locals
        ) :: Maybe TableDef
  forceMaybe' ("Table for alias '" <> alias <> "' was not found") mdef

lookupField :: String -> String -> TableDefLookup -> Maybe FieldDef
lookupField table field s = let
  tableDef = lookupTable table s
  in Map.lookup field tableDef.fields

-- Build symbol lookups for known tables in the FROM 
buildTableLookup :: forall m. MonadEffect m => 
  External_ -> SelectTree -> m (Maybe (TableLookup))
buildTableLookup ex tree = do
  coll <- newMap
  validate ex do
    addAllTables coll
    
  ifM (hasErrors ex) (do
    pure Nothing
  ) (do 
    Just <$> aread coll 
  )
  
  where
  -- Go through the entire 'from' segment and document the tables.
  addAllTables :: MapColl String String -> Validate m Unit
  addAllTables coll = do
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
        -- TODO -- subqueries are added to Lookup and LocalTables separately.
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

-- Build local tables and lookups for subqueries declared in the FROM clause.
buildLocalTables :: forall m. MonadEffect m =>
  External_ -> TableLookup -> SelectTree -> m (Maybe (TableLookup /\ LocalTables))
buildLocalTables ex tables tree = do
  case tree.from of
    P.Tables _ -> do
      pure $ Just $ tables /\ Map.empty
    P.Joins join -> do 
      lookups <- newMap
      awrite tables lookups
      locals <- newMap
      validate ex do 
        buildJoin join lookups locals

      ifM (hasErrors ex) (do
        pure Nothing 
      ) (do 
        lookups' <- aread lookups
        locals' <- aread locals
        pure $ Just $ lookups' /\ locals'
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

-- Build a unified API for looking up the tables. Verifies that all tables
-- are already defined externally or locally, and if they are, returns a 
-- way to lookup tables definitions.
buildTableDefLookup :: forall m. MonadEffect m =>
  External_ -> TableLookup -> LocalTables -> m (Maybe (TableDefLookup))
buildTableDefLookup ex lookups locals = do
  validate ex do
    for_ tableSymbols \(alias /\ name) -> do
      assert (hasTableDef name) $ noTableMsg alias name
      
  ifM (hasErrors ex) (do
    pure Nothing
  ) (do 
    -- If we obtained no errors, then we are free to assume that all the table
    -- definitions exist, and will always exist.
    pure $ Just { ex, locals, aliases: lookups }
  )
  
  where
  tableSymbols = Map.toUnfoldable lookups :: Array _

  hasTableDef name = tableExists ex name || Map.member name locals 

  noTableMsg alias name = 
    "Alias '" <> alias <> "' for table '" <> name <> "' does not have a "
      <> "matching global table, " <> "or local subquery"

-- Verify that all uses of tables and fields, in all clauses, are valid.
checkTablesAndFields :: forall m. MonadEffect m => 
  External_ -> TableDefLookup -> SelectTree -> m Boolean
checkTablesAndFields ex defs tree = do
  validate ex do
    checkSelect
    checkFrom
    checkWhere
    checkGroupBy
    checkOrderBy
    checkLimit
  notM (hasErrors ex)
  
  where 
  todo = assert false "TODO"

  checkSelect = todo
  checkFrom = todo
  checkWhere = todo
  checkGroupBy = todo
  checkOrderBy = todo
  checkLimit = todo

validate :: forall m. MonadEffect m => External_ -> Validate m Unit -> m Unit
validate ex prog = do runValidate_ (warn ex) prog

class External a where
  tableDef :: a -> String -> Maybe TableDef
  tableExists :: a -> String -> Boolean
  warn :: forall m. MonadEffect m => a -> String -> m Unit
  hasErrors :: forall m. MonadEffect m => a -> m Boolean

newtype External_ = External__ (forall r. (forall z. External z => z -> r) -> r)

wrap :: forall z. External z => z -> External_
wrap z = External__ (_ $ z)

instance External (External_) where 
  tableDef (External__ run) = run tableDef
  tableExists (External__ run) = run tableExists
  warn (External__ run) = run warn
  hasErrors (External__ run) = run hasErrors
