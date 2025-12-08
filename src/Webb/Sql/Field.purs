module Webb.Sql.Field where

import Prelude


type Field = 
  { name :: String
  , attrs :: Array Attr
  }
  
data Attr = Nil | FieldType FieldType | Primary | Unique | Foreign ForeignKey

data FieldType = Integer
  | Text
  | Real
  | Bool
  
type ForeignKey = { table :: String, field :: String }
  
type FieldM' a = State Field a
type FieldM = FieldM' Unit

empty :: Field
empty = { name: "", attrs: [] }
  
-- Constructs a newtype of _specific_ field operations -- that become table operations.
field :: String -> FieldM -> Field
field name (F prog) = let 
  f = execState prog $ empty { name = name }
  in f
  
addLast :: Attr -> FieldM 
addLast attr = do
  mmodify_ $ \s -> s { attrs = A.snoc s.attrs attr }
  
fieldType :: FieldType -> FieldM
fieldType f = addLast $ FieldType f

int :: FieldM
int = fieldType Integer

string :: FieldM
string = fieldType Text

number :: FieldM
number = fieldType Real

bool :: FieldM
bool = fieldType Bool

nil :: FieldM
nil = addLast Nil

unique :: FieldM
unique = addLast Unique

primaryKey :: FieldM
primaryKey = addLast Primary

foreignKey :: String -> FieldM
foreignKey spec = do
  let splits = String.split (Pattern ".") spec
      table = unsafeIndex 0
      field = unsafeIndex 1
  addLast $ Foreign { table, field }




  
    