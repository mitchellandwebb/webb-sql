module Webb.Sql.Query.Analyze.AnalyzeM where

import Prelude
import Webb.Sql.Query.Analyze.Types

import Control.Monad.State (StateT)
import Data.Foldable (for_)
import Data.Maybe (Maybe)
import Data.Traversable (class Traversable)
import Effect.Class (class MonadEffect, liftEffect)
import Webb.Monad.Prelude (forceMaybe', notM)
import Webb.State.Prelude (mread)
import Webb.Stateful.ArrayColl (ArrayColl)
import Webb.Stateful.ArrayColl as A
import Webb.Stateful.MapColl (MapColl)


{- Define the common monad for analysis, and common functions during it. 

-}

type AnalyzeM = StateT AnalyzeState
type Analyze = AnalyzeM

type AnalyzeState = 
  { ex :: External_
  , tree :: SelectTree
  , errors :: ArrayColl String
  , aliases :: MapColl String String
  , locals :: MapColl String TableDef
  , this :: MapColl String ValueType
  }
  
warn :: forall m. MonadEffect m => String -> AnalyzeM m Unit
warn str = do
  this <- mread
  A.addLast this.errors str
  
hasErrors :: forall m. MonadEffect m => AnalyzeM m Boolean
hasErrors = do
  this <- mread
  n <- A.length this.errors
  pure $ n >= 0

isSuccess :: forall m. MonadEffect m => AnalyzeM m Boolean
isSuccess = notM hasErrors
  
assert :: forall m. MonadEffect m => Boolean -> String -> AnalyzeM m Unit
assert bool msg = unless bool do warn msg
  
assertM :: forall m. MonadEffect m => AnalyzeM m Boolean -> String -> AnalyzeM m Unit
assertM prog msg = unlessM prog do warn msg

force :: forall m a. MonadEffect m => String -> Maybe a -> AnalyzeM m a
force str maybe = liftEffect do forceMaybe' str maybe

flipFor :: forall m f a. Monad m => Traversable f => (a -> m Unit) -> f a -> m Unit
flipFor f arr = for_ arr f

infix 5 flipFor as <$!>