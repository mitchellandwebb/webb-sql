module Webb.Sql.Query.Analyze where

import Prelude

import Data.Map as Map
import Data.Map (Map)
import Webb.Sql.Query.Parser as P


{- Analyze the proposed query, turning each clause into more helpful forms for comparisons against each other, and comparisons against the defined table's types.
We can thus type-check fields, but also type-generate the query -- what do we
expect the type of the object to be; what labels will it have, and what types?

We have the Query object. But what do we want to accomplish? What kinds of analyses need to occur? Well, a few things:

- Obtain knowledge about the tables. What tables are being used? What are their aliases, or do they retain their original names? This lets us quickly check that tables exist, and that fields exist, and to obtain their types. In other words, we can obtain the _used symbols_ within this context, and also assert that the same symbol does NOT occur twice, ever, as a placeholder for a table.
- Obtain knowledge about functions that are used. Things like Max, Min, and Count are functions that operate on fields-as-named entities to produce booleans, others like PLUS and LIKE are true operators on fields-as-data-references to produce output values. In fact, it's to the extent that Aggregator functions like Max and Min _change_ the output context entirely -- that is, GroupBy does a weird type change to all the rows that exist after (Select ... From ... Where), and so this information is needed before we can properly analyze the columns. That is, each table included in FROM, will add to the known columns that are possible for the query's output, in some sense or another. But it's this context that lets us known whether the SELECT context has something to operate on. So it's ... conceivable, then, that we are trying to load in, and transform, the lookup table of symbols for the query. GROUP BY thus represents a transformation of the table from a flat table of fields, to a GROUP type consisting of the grouped fields, and a list of value fields to aggregate on that cannot be shown, but CAN be acted on by aggregate fields; a MAX(field) claim is thus implicitly acting on implied, lazy aggregates that are inherited from the group type; or a MAX(field) is implicitly a MAX(group, field as String). So we have to account for this when type-checking -- there's a strange set of transformations that must occur.
- With symbols, we also want to validate the correctness of table and field references. It's a pre-requisite to doing any analysis of types to begin with -- all types must be present and known. Thus, we want to check that all referenced fields belong to a table, or to a global. If a name occurs on its own, without a table, in the position of a field, then it must belong to a _singular_ table; if not, then a singular field is impossible. Likewise, if a field occurs with a table, we must validate that both the table and the field are _known_ names to the database schema. And when _function calls_ are made, we must ensure these are known functions. Once we know that _all_ arbitrary names refer to valid items, only then does it make sense to type-check them against each other -- we separate in phases so that the error publishing for each phase can be separate.
- We also need _recursive expression_ verification. This is not about types (or maybe it is?). We have some odd constructions that go _beyond_ types -- like 'SELECT *', or 'SELECT COUNT(*)', which are valid even though 'SELECT * + *' is not valid. This is sort-of type-checking, but also sort-of not. Likewise with 'SELECT * all, customerId' is nonsensical, because aliasing an asterisk ... makes no sense, even though * can indeed serve as a value to some functions. The FORM is mimicked, but the contextual meaning of the form can only be deduced through type-checking -- type-checking of a more expansive kind than just Integer, Real, Text, Bool, and so on. Indeed, the '*' does not have a clear type operator, in that sense. Where 'COUNT(*)' makes sense, 'MAX(*)' doesn't really make sense. So we want to be able to express that. And 'SELECT *' edits the column output in ways that aren't ... typed. So what exactly is going on here?

-}


data ValueType 
  = Integer
  | Real
  | Text
  | Bool
  | Field
  
type RecordType = Map String ValueType

