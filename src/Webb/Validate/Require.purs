module Webb.Validate.Require where

import Prelude

import Data.Array (length)
import Effect.Class (class MonadEffect, liftEffect)
import Webb.Monad.Prelude (expect, expectM, forceMaybe')
import Webb.State.Prelude (aread)
import Webb.Stateful.ArrayColl (ArrayColl, newArray)
import Webb.Stateful.ArrayColl as A

{- Repositories of values. Can specifically require 1, or a given number. 

Why is this difficult? If this were Smalltalk, it would be easy.

It's fascinating -- in Smalltalk, I'd just be telling the code what to do.
Why is so different here? The distance from intent to result is surprisingly high.
Normally I'd just have a method for sending a message. Asserting something
about the the data, which is carrying a message, and then do something. Yet what
I'm finding now is complete blockage of the mind. Inability to generate useful data.
That's what is really happening. And trying harder won't overcome it, surprisingly
enough.
-}


newtype Require a = RQ { name :: String, coll :: (ArrayColl a) }

newRequire :: forall m a. MonadEffect m => String -> m (Require a)
newRequire name = do
  arr <- newArray
  pure $ RQ { name, coll: arr }
  
addLast :: forall m a. MonadEffect m => Require a -> a -> m Unit
addLast (RQ s) a = do A.addLast s.coll a

expectAtMost :: forall m a. MonadEffect m => Require a -> Int -> m Unit
expectAtMost (RQ s) n = liftEffect do
  len <- A.length s.coll
  expect (len == n) $ 
    "Needed at most " <> show n <> " values for " <> 
      s.name <> ", but got " <> show len

requireN :: forall m a. MonadEffect m => Require a -> Int -> m (Array a)
requireN (RQ s) n = liftEffect do
  arr <- A.removeFirstN s.coll n
  expect (length arr == n) $ 
    "Required " <> show n <> " values for " <> 
      s.name <> ", but got " <> show (length arr)
  pure arr

optionalN :: forall m a. MonadEffect m => Require a -> m a
optionalN (RQ s) = liftEffect do
  mfirst <- A.removeFirst s.coll
  forceMaybe' ("Missing the required value for " <> s.name) mfirst