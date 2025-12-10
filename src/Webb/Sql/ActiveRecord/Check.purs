module Webb.Sql.ActiveRecord.Check where

import Prelude

{- When we create an active record, we are implicitly creating a SQL query tree that relies on existing definitions of the table. Thus, we need to type-check the query against existing tables. An active record's definition also assigns existing fields to public active fields; for that reason, we also need to typecheck these fields as well. This thus requires the Tables to already all be defined, parsed, and standardized, and type-checked, so that we can rely on querying data from them. However, we also need
to _parse_ the SQL queries into data forms that we can use. This is not standard SQL. In particular, we include a `this` keyword that helps define whether the query has an input argument; the `this` keyword's are then typed by inference from how it is used in the query -- against which fields, and which constants, and for which operations. This leads to a limited amount of type-checking -- but this type-checking can only take place after parsing it into an AST. The type-checking that we want to perform therefore belongs to a separate module that we import -- the Query module. There's
no reason for us to do it in this module; instead, we parse the SQL string and use the other Query module's definitions to validate it against the tables.
-}