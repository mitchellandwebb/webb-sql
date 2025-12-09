module Webb.Sql.Field where

import Prelude

import Control.Monad.State (State, execState)
import Data.Array as A
import Data.Maybe (Maybe(..), isJust)
import Unsafe.Coerce (unsafeCoerce)
import Webb.State.Prelude (mmodify_)

type Field = 
  { name :: String
  , attrs :: Array Attr
  }
  
data Attr = Nil 
  | AType FieldType 
  | Primary 
  | Unique 
  | Foreign ForeignRef 
  | Default Void

data FieldType = Integer
  | Text
  | Real
  | Bool
  
type ForeignRef = { table :: String, field :: String }
  
type FieldM' a = State Field a
type FieldM = FieldM' Unit


empty :: Field
empty = { name: "", attrs: [] }
  
-- Constructs a newtype of _specific_ field operations -- that become table operations.
field :: String -> FieldM -> Field
field name prog = let 
  f = execState prog $ empty { name = name }
  in f
  
addLast :: Attr -> FieldM 
addLast attr = do
  mmodify_ $ \s -> s { attrs = A.snoc s.attrs attr }
  
fieldType :: FieldType -> FieldM
fieldType f = addLast $ AType f

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

default :: forall a. a -> FieldM
default v = addLast $ Default (unsafeCoerce v) -- We will check this at runtime.

primaryKey :: FieldM
primaryKey = addLast Primary

foreignKey :: String -> String -> FieldM
foreignKey table field' = do
  addLast $ Foreign { table, field: field' }

isPrimary :: Field -> Boolean
isPrimary { attrs } = isJust $ A.find isPrimary_ attrs 
  where 
  isPrimary_ attr = case attr of
    Primary -> true
    _ -> false
    
toPrimary :: Field -> Maybe (Array String)
toPrimary self@{ name } = 
  if isPrimary self then
    Just [ name ]
  else 
    Nothing

  
toForeignKeys :: Field -> Array { name :: String, ref :: ForeignRef }
toForeignKeys { name, attrs } = let 
  foreigns = attrs <#> \attr -> case attr of
    Foreign ref -> Just { name, ref }
    _ -> Nothing
  in A.catMaybes foreigns