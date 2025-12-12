module Webb.Sql.Query.Analyze where

import Prelude

import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String as Str
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Class (class MonadEffect)
import Webb.Sql.Query.Parser (From(..), TableData)
import Webb.Sql.Query.Parser as P
import Webb.Sql.Query.Token (Token)
import Webb.State.Prelude (aread, awrite)
import Webb.Stateful.MapColl (MapColl, newMap)
import Webb.Stateful.MapColl as M
import Webb.Validate.Validate (Validate, assert, runValidate_)


{- Analyze the proposed query, turning each clause into more helpful forms for comparisons against each other, and comparisons against the defined table's types.
We can thus type-check fields, but also type-generate the query -- what do we
expect the type of the object to be; what labels will it have, and what types?

We have the Query object. But what do we want to accomplish? What kinds of analyses need to occur? Well, a few things:

- Obtain knowledge about the tables. What tables are being used? What are their aliases, or do they retain their original names? This lets us quickly check that tables exist, and that fields exist, and to obtain their types. In other words, we can obtain the _used symbols_ within this context, and also assert that the same symbol does NOT occur twice, ever, as a placeholder for a table.
- Obtain knowledge about functions that are used. Things like Max, Min, and Count are functions that operate on fields-as-named entities to produce booleans, others like PLUS and LIKE are true operators on fields-as-data-references to produce output values. In fact, it's to the extent that Aggregator functions like Max and Min _change_ the output context entirely -- that is, GroupBy does a weird type change to all the rows that exist after (Select ... From ... Where), and so this information is needed before we can properly analyze the columns. That is, each table included in FROM, will add to the known columns that are possible for the query's output, in some sense or another. But it's this context that lets us known whether the SELECT context has something to operate on. So it's ... conceivable, then, that we are trying to load in, and transform, the lookup table of symbols for the query. GROUP BY thus represents a transformation of the table from a flat table of fields, to a GROUP type consisting of the grouped fields, and a list of value fields to aggregate on that cannot be shown, but CAN be acted on by aggregate fields; a MAX(field) claim is thus implicitly acting on implied, lazy aggregates that are inherited from the group type; or a MAX(field) is implicitly a MAX(group, field as String). So we have to account for this when type-checking -- there's a strange set of transformations that must occur.
- With symbols, we also want to validate the correctness of table and field references. It's a pre-requisite to doing any analysis of types to begin with -- all types must be present and known. Thus, we want to check that all referenced fields belong to a table, or to a global. If a name occurs on its own, without a table, in the position of a field, then it must belong to a _singular_ table; if not, then a singular field is impossible. Likewise, if a field occurs with a table, we must validate that both the table and the field are _known_ names to the database schema. And when _function calls_ are made, we must ensure these are known functions. Once we know that _all_ arbitrary names refer to valid items, only then does it make sense to type-check them against each other -- we separate in phases so that the error publishing for each phase can be separate.
- We also need _recursive expression_ verification. This is not about types (or maybe it is?). We have some odd constructions that go _beyond_ types -- like 'SELECT *', or 'SELECT COUNT(*)', which are valid even though 'SELECT * + *' is not valid. This is sort-of type-checking, but also sort-of not. Likewise with 'SELECT * all, customerId' is nonsensical, because aliasing an asterisk ... makes no sense, even though * can indeed serve as a value to some functions. The FORM is mimicked, but the contextual meaning of the form can only be deduced through type-checking -- type-checking of a more expansive kind than just Integer, Real, Text, Bool, and so on. Indeed, the '*' does not have a clear type operator, in that sense. Where 'COUNT(*)' makes sense, 'MAX(*)' doesn't really make sense. So we want to be able to express that. And 'SELECT *' edits the column output in ways that aren't ... typed. So what exactly is going on here? How does this become a validation pipeline, and then an output pipeline?
- We also have to type-check many clauses, particular orders. The GroupBy must refer to real fields, as part of the field check. So must OrderBy; in particular, OrderBy must be able to order by the implicit aggregates of the GroupedRecord. But a big part of type-checking is of the JOINS and of the WHERE, which contain multiple Boolean clauses. The parse of these value expressions necessarily EXCLUDES certain illegal expressions -- aggregate functions are NOT available in the JOINs and WHERE, which are entirely part of the selection phase, and Wildcards are NOT valid expressions. And we would like to perform this type-checking in a generalized way. Thus, it might be necessary to give things like '*' and 'table.*' a proper type, and for the aggregators to have implicit 'union' types that can understand multiple types.
-  What's interesting is that the type-checking doesn't necessarily translate to any real runtime -- only to code generation. And what I mean by this is is that we aren't really compiling to any machine code that will actually run. Instead, we're asserting that _if_ an expression type-checks, then its compile-time output representation will also be valid at runtime, in its local runtime context. But what determines the output-representation of a symbol in a context? And will it always be correct? In my case ... the representations are close to 1-1. But that isn't necessarily the case in other languages -- but here, I think we're fairly safe. We needn't analyze the expression's type to determine how to output a '*' differently in one place vs another -- it will be an asterisk in both cases. But otherwise, we would have to rely on the type and context to output the field.

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
    ifM (isJust <$> M.lookup coll name) (do 
      assert false $ "Table alias is already used: " <> name
    ) (do 
      M.insert coll name table
    )

-- Build local tables from subqueries declared in the FROM clause.
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
        assert false "How to build a subquery definition is not yet defined"

validate :: forall m. MonadEffect m => External_ -> Validate m Unit -> m Unit
validate ex prog = do runValidate_ (warn ex) prog

class External a where
  warn :: forall m. MonadEffect m => a -> String -> m Unit
  hasErrors :: forall m. MonadEffect m => a -> m Boolean

newtype External_ = External__ (forall r. (forall z. External z => z -> r) -> r)

wrap :: forall z. External z => z -> External_
wrap z = External__ (_ $ z)

instance External (External_) where 
  warn (External__ run) = run warn
  hasErrors (External__ run) = run hasErrors
