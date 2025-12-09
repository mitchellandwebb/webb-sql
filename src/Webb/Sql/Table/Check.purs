module Webb.Sql.Table.Check where

import Prelude

import Data.Array as A
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (for)
import Effect.Class (class MonadEffect)
import Webb.Sql.Table.Field (FieldType)
import Webb.Sql.Table.Field as F
import Webb.Sql.Table.Table as T
import Webb.State.Prelude (aread, areads, awrite, newShowRef)
import Webb.Stateful (localEffect)
import Webb.Stateful.MapColl (newMap)
import Webb.Stateful.MapColl as M
import Webb.Stateful.SetColl (newSet)
import Webb.Stateful.SetColl as S
import Webb.Validate.Validate (Validate, assert, atMostOne, exactlyOne, noDuplicates, runValidate_, takeAll, takeFirst, takeFirstMaybe, takeIsNotEmpty)


{- Once we have a table, we have to validate it. 
That typically means organizing the data into explicit forms that
are useful for validation.
-}

class External a where
  columnExists :: a -> String -> String -> Boolean
  tableExists :: a -> String -> Boolean
  warn :: forall m. MonadEffect m => a -> String -> m Unit
  hasErrors :: forall m. MonadEffect m => a -> m Boolean
  primaryKey :: a -> String -> Array String
  fieldType :: a -> String -> String -> F.FieldType

newtype External_ = External__ (forall r. (forall z. External z => z -> r) -> r)

wrap :: forall z. External z => z -> External_
wrap z = External__ (_ $ z)

instance External (External_) where 
  columnExists (External__ run) = run columnExists
  tableExists (External__ run) = run tableExists
  warn (External__ run) = run warn
  hasErrors (External__ run) = run hasErrors
  primaryKey (External__ run) = run primaryKey
  fieldType (External__ run) = run fieldType

type FinalTable = 
  { -- A table has one primary king
    primaryKey :: Array String
    
    -- A table can have multiple foreign keys.
  , foreignKeys :: Array ForeignKey
  
    -- A table can have multiple fields.
  , fields :: Array FinalField
  , name :: String
  }
  
type FinalField = 
  { name :: String
  , kind :: FieldType -- We will abort if a field type wasn't provided.
  , nil :: Boolean
  , default :: Maybe Void
  , unique :: Boolean
  }
  
type ForeignKey = 
  { fields :: Array String
  , ref :: { table :: String, fields :: Array String }
  }
  
type PrimaryKey = Array String
  
-- Initial validation pass. If it passes, we can generate
-- the initial structure of the table definition.
validate1 :: forall m. MonadEffect m => 
  External_ -> T.Table -> m (Maybe FinalTable)
validate1 ex table = do
  validate ex do
    assert (tableExists ex table.name) "Table does not exist"

  let primaryKeys = getPrimaryKeys
  validate ex do
    noDuplicates primaryKeys "Primary keys have duplicates"
    exactlyOne primaryKeys "There can only be one primary key"
    
  let foreignKeys = getForeignKeys
  validate ex do
    noDuplicates foreignKeys "Foreign keys have duplicates"
    assert (noOverlaps foreignKeys) "Foreign keys have overlapping fields"
    
  for_ table.fields \field -> do
    validate ex do 
      exactlyOne field.kind  
        $ "Need exactly one field type for field: " <> field.name
      atMostOne field.unique
        $ "Need exactly one 'unique' declaration for field: " <> field.name
      atMostOne field.unique
        $ "Need exactly one 'nil' declaration for field: " <> field.name
      atMostOne field.default 
        $ "Need at most one default value for field: " <> field.name
        
  ifM (hasErrors ex) (do 
    pure Nothing 
  ) (do 
    primaryKey <- takeFirst primaryKeys
    foreigns <- takeAll foreignKeys
    fields <- for table.fields \field -> do 
      kind <- takeFirst field.kind
      nil <- takeIsNotEmpty field.nil
      unique <- takeIsNotEmpty field.unique
      default <- takeFirstMaybe field.default
      pure { name: field.name, kind, nil, unique, default }

    pure $ Just { name: table.name, primaryKey, foreignKeys: foreigns, fields }
  )
  
  where 
  getPrimaryKeys :: Array PrimaryKey
  getPrimaryKeys = let
    fromFields = do
      field <- table.fields
      field.primaryKey <#> \_ -> [field.name]
    fromTable = table.primaryKeys
    in fromTable <> fromFields
    
  getForeignKeys :: Array ForeignKey
  getForeignKeys = let
    fromFields = do
      field <- table.fields
      foreign_ <- field.foreignKey
      pure $ { 
        fields: [ field.name ]
        , ref: { table: foreign_.table, fields: [ foreign_.field ] } 
        }
    fromTable = table.foreignKeys
    in fromTable <> fromFields
    
  noOverlaps :: Array ForeignKey -> Boolean
  noOverlaps arr = localEffect do
    set <- newSet
    hasOverlap <- newShowRef false
    for_ arr \key -> do
      for_ key.fields \field -> do
        whenM (S.member set field) do
          awrite true hasOverlap
        S.insert set field
    areads not hasOverlap
      

-- Second pass over the table validation, checking things that are
-- only possible now that we've standardized the tables.
validate2 :: forall m. MonadEffect m => 
  External_ -> FinalTable -> m (Maybe FinalTable)
validate2 ex table = do
  validate ex do 
    assert tableNameExists table.name

    assert (primaryKeyExists) $ 
      "Primary key " <> show table.primaryKey <> " uses non-existent fields"
    assert (primaryIsNotForeign) $ 
      "Primary key cannot be a foreign key"

    for_ table.foreignKeys \key -> do
      assert (foreignKeyExists key) $ 
        "Foreign key " <> show key.fields <> " uses non-existent fields"
      assert (foreignRefExists key) $ 
        "Foreign key refers to invalid referent: " <> show key
      assert (foreignRefIsPrimary key) $ 
        "Foreign key does not refer to primary key: " <> show key
      assert (foreignKeyMatchesTypes key) $ 
        "Foreign key's field types do not match the foreign table: " <> show key
        
    for_ table.fields \field -> do
      validDefault field
      
  ifM (hasErrors ex) (do 
    pure Nothing 
  ) (do 
    pure $ Just table
  )
  
  where
  tableNameExists = tableExists ex table.name

  primaryKeyExists = let 
    fields = table.primaryKey
    in A.all (\field -> columnExists ex table.name field) fields
    
  primaryIsNotForeign = let
    set = Set.fromFoldable (table.foreignKeys <#> \f -> f.fields)
    in not $ Set.member table.primaryKey set
    
  foreignKeyExists key = 
    A.all (\field -> columnExists ex table.name field) key.fields
    
  foreignRefExists key = 
    A.all (\field -> columnExists ex key.ref.table field) key.ref.fields
    
  foreignRefIsPrimary key = 
    primaryKey ex key.ref.table == key.ref.fields
    
  foreignKeyMatchesTypes key = 
    let localTypes = (localEffect do 
          map <- newMap
          for_ key.fields \field -> do
            let kind = fieldType ex table.name field
            M.insert map field kind
          aread map) :: Map String F.FieldType
        foreignTypes = (localEffect do 
          map <- newMap
          for_ key.ref.fields \field -> do
            let kind = fieldType ex key.ref.table field
            M.insert map field kind
          aread map) :: Map String F.FieldType
    in localTypes == foreignTypes

  -- Assert for the field that the field has a default matching its type
  validDefault field = do
    case field.default of
      Nothing -> pure unit
      Just default -> do
        case field.kind of
          F.Integer -> assert (isInteger default) $ 
            "Integer field '" <> field.name <> "' has invalid default" 
          F.Text -> assert (isString default) $ 
            "Text field '" <> field.name <> "' has invalid default" 
          F.Real -> assert (isNumber default) $ 
            "Real field '" <> field.name <> "' has invalid default" 
          F.Bool -> assert (isBoolean default) $ 
            "Bool field '" <> field.name <> "' has invalid default" 
  
validate :: forall m. MonadEffect m => External_ -> Validate m Unit -> m Unit
validate ex prog = runValidate_ (warn ex) prog

foreign import isInteger :: forall a. a -> Boolean
foreign import isString :: forall a. a -> Boolean
foreign import isNumber :: forall a. a -> Boolean
foreign import isBoolean :: forall a. a -> Boolean