module Webb.Sql.ValidatedTable where

import Prelude

import Data.Array as A
import Data.Foldable (for_)
import Data.Maybe (Maybe, isJust)
import Data.Traversable (for)
import Webb.Sql.Field (FieldType)
import Webb.Sql.Field as F
import Webb.Sql.Table as T
import Webb.Sql.Validate (Validate, atMostOne, fatal, validating)


{- Once we have a table, we have to validate it. 
That typically means organizing the data into explicit forms that
are useful for validation.
-}


type Table = 
  { -- A table has one primary king
    primaryKey :: Array String
    
    -- A table can have multiple foreign keys.
  , foreignKeys :: Array ForeignKey
  
    -- A table can have multiple fields.
  , fields :: Array Field
  }
  
type Field = 
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
  
{-
fromTable :: T.Table -> Validate Table
fromTable t = do
      
  fatal "blah"
  
  where
  primaryKeys = validating "primary key" do 
    let primaryFields = A.catMaybes $ F.toPrimary <$> t.fields
        all = t.primaryKeys <> primaryFields
    atMostOne all
    
  foreignKeys = validating "foreign keys" do 
    let foreignFields = join $ F.toForeignKeys <$> t.fields
        foreignFieldKeys = foreignFields <#> \s ->
          { fields: [s.name ]
          , ref: { table: s.ref.table, fields: [ s.ref.field ]}
          } 
        all = t.foreignKeys <> foreignFieldKeys 
    noDuplicates all
    noOverlaps all
    pure all
    
  fields = validating "fields" do 
    fields' <- for t.fields \field -> do
      kind <- exactlyOne' "type" $ A.catMaybes $ F.toType <$> field.attrs
      mnil <- atMostOne' "nil" $ A.catMaybes $ F.toNil <$> field.attrs
      mdefault <- atMostOne' "default" $ A.catMaybes $ F.toDefault <$> field.attrs
      munique <- atMostOne' "unique" $ A.catMaybes $ F.toUnique <$> field.attrs

      pure 
        { name: field.name
        , kind: kind
        , nil: isJust mnil
        , default: mdefault
        , unique: isJust munique
        }
      
    noDuplicates' "name" $ _.name <$> fields'
    pure fields'

-}
