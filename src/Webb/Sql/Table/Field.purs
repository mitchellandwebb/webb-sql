module Webb.Sql.Table.Field where

import Prelude

import Control.Monad.State (State, execState)
import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', over)
import Data.Lens.Record (prop)
import Data.Show.Generic (genericShow)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Webb.State.Prelude (mmodify_)

type Field = 
  { name :: String
  , primaryKey :: Array Unit
  , kind :: Array FieldType
  , nil :: Array Unit
  , unique :: Array Unit
  , foreignKey :: Array ForeignRef
  , default :: Array Void
  }

_name :: forall a r. Lens' { name :: a | r } a
_name = prop (Proxy :: Proxy "name")

_nil :: forall a r. Lens' { nil :: a | r } a
_nil = prop (Proxy :: Proxy "nil")

_primaryKey :: forall a r. Lens' { primaryKey :: a | r } a
_primaryKey = prop (Proxy :: Proxy "primaryKey")

_kind :: forall a r. Lens' { kind :: a | r } a
_kind = prop (Proxy :: Proxy "kind")

_unique :: forall a r. Lens' { unique :: a | r } a
_unique = prop (Proxy :: Proxy "unique")

_foreignKey :: forall a r. Lens' { foreignKey :: a | r } a
_foreignKey = prop (Proxy :: Proxy "foreignKey")

_default :: forall a r. Lens' { default :: a | r } a
_default = prop (Proxy :: Proxy "default")
  
data FieldType = Integer
  | Text
  | Real
  | Bool
  
derive instance Eq FieldType
derive instance Ord FieldType
derive instance Generic FieldType _
instance Show FieldType where show = genericShow
  
type ForeignRef = { table :: String, field :: String }
  
type FieldM' a = State Field a
type FieldM = FieldM' Unit

empty :: Field
empty = 
  { name: "", primaryKey: [], foreignKey: []
  , unique: [], kind: [], default: [] 
  , nil: []
  }
  
-- Constructs a newtype of _specific_ field operations -- that become table operations.
field :: String -> FieldM -> Field
field name prog = let 
  f = execState prog $ empty { name = name }
  in f
  
addLast :: forall a. Lens' Field (Array a) -> a -> FieldM 
addLast lens a = do
  mmodify_ $ over lens (\arr -> A.snoc arr a) 
  
fieldType :: FieldType -> FieldM
fieldType f = addLast _kind f

int :: FieldM
int = fieldType Integer

string :: FieldM
string = fieldType Text

number :: FieldM
number = fieldType Real

bool :: FieldM
bool = fieldType Bool

nil :: FieldM
nil = addLast _nil unit

unique :: FieldM
unique = addLast _unique unit

default :: forall a. a -> FieldM
default v = addLast _default (unsafeCoerce v) -- We will check this at runtime.

primaryKey :: FieldM
primaryKey = addLast _primaryKey unit

foreignKey :: String -> String -> FieldM
foreignKey table field' = do
  addLast _foreignKey { table, field: field' }

isPrimary :: Field -> Boolean
isPrimary s = A.length s.primaryKey > 0
    