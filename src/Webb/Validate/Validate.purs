module Webb.Validate.Validate where

import Prelude

import Control.Monad.State (StateT, runStateT)
import Data.Array as A
import Data.Foldable (for_)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Webb.Monad.Prelude (forceMaybe', notM)
import Webb.State.Prelude (aread, awrite, mread, newShowRef)
import Webb.Stateful.SetColl (newSet)
import Webb.Stateful.SetColl as S




type Validate m a = StateT (String -> Effect Unit) m a

runValidate_ :: forall m. Monad m => 
  (String -> Effect Unit) -> Validate m Unit -> m Unit
runValidate_ f prog = void $ runStateT prog f

assert :: forall m. MonadEffect m => Boolean -> String -> Validate m Unit
assert success msg = do
  unless success do 
    s <- mread 
    s msg  # liftEffect

assertM :: forall m. MonadEffect m => Effect Boolean -> String -> Validate m Unit
assertM success msg = do
  unlessM (liftEffect success) do 
    s <- mread 
    s msg  # liftEffect
    
exactlyOne :: forall m a. MonadEffect m => Array a -> String -> Validate m Unit
exactlyOne arr msg = do
  assert (A.length arr == 1)  msg

atMostOne :: forall m a. MonadEffect m => Array a -> String -> Validate m Unit
atMostOne arr msg = do
  assert (A.length arr <= 1)  msg
  
noDuplicates :: forall m a. MonadEffect m => Ord a => 
  Array a -> String -> Validate m Unit
noDuplicates arr msg = do
  set <- newSet
  hasDuplicates <- newShowRef false
  for_ arr \a -> do
    whenM (S.member set a) do
      awrite true hasDuplicates
    S.insert set  a

  assertM (notM $ aread hasDuplicates) msg

takeFirst :: forall m a. MonadEffect m => Array a -> m a
takeFirst arr = liftEffect do
  forceMaybe' "No first element" $ A.head arr
  
takeAll :: forall m a. MonadEffect m => Array a -> m (Array a)
takeAll arr = pure arr

takeIsNotEmpty :: forall m a. MonadEffect m => Array a -> m Boolean
takeIsNotEmpty arr = do pure $ A.length arr > 0

takeFirstMaybe :: forall m a. MonadEffect m => Array a -> m (Maybe a)
takeFirstMaybe arr = pure $ A.head arr