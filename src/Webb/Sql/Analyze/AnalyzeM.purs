module Webb.Sql.Analyze.AnalyzeM where

import Prelude
import Webb.Sql.Analyze.Types

import Control.Monad.State (StateT)
import Effect.Class (class MonadEffect)
import Webb.State.Prelude (mread)
import Webb.Stateful.ArrayColl (ArrayColl)
import Webb.Stateful.ArrayColl as A
import Webb.Stateful.MapColl (MapColl)


{- Define the common monad for analysis, and common functions during it. -}

type AnalyzeM = StateT AnalyzeState

type AnalyzeState = 
  { ex :: External_
  , tree :: SelectTree
  , errors :: ArrayColl String
  , aliases :: MapColl String String
  , locals :: MapColl String TableDef
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
  
assert :: forall m. MonadEffect m => Boolean -> String -> AnalyzeM m Unit
assert bool msg = unless bool do warn msg
  
assertM :: forall m. MonadEffect m => AnalyzeM m Boolean -> String -> AnalyzeM m Unit
assertM prog msg = unlessM prog do warn msg

  