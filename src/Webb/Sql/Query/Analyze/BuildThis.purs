module Webb.Sql.Query.Analyze.BuildThis where

import Prelude
import Webb.Sql.Query.Analyze.AnalyzeM
import Webb.Sql.Query.Analyze.Types

import Effect.Class (class MonadEffect)
import Webb.Monad.Prelude (notM)


{- Build the type of the 'this' object, by examining the type it is being used
  against in expressions to deduce its ultimate type, and ultimately publishing a warning
  if any property of 'this' is used such that we cannot determine its type.
-}


buildThis :: forall m. MonadEffect m => Analyze m Boolean
buildThis = do
  checkSelect
  checkFrom
  checkWhere
  checkGroupBy
  checkOrderBy

  notM hasErrors
  
  where
  checkSelect = pure unit
  checkFrom = pure unit
  checkWhere = pure unit
  checkGroupBy = pure unit
  checkOrderBy = pure unit
  
{- To infer the type of something like 'this', what can we do? How does type inference
  work in general? What is the most effective way of describing and solving this problem?
  The problem with solving this with pure functions, is that a 'this' represents a hole,
  and we'd like to appeal to the context to determine the type. But there is, potentially,
  _no_ context. But is that necessarily so?
-}

{- 
The proper with static typing is that it interferes with running the code. It enables amazing things, but it also makes some things impossible to say, or impossible to automate. And clojure's debugger situation is so shit that I don't want to use it. Smalltalk it is, then. But what of the problem's with Smalltalk? What of the trouble with writing Async code, and of quickly compiling to JS, and of having sane incremental compilation? What of the slowness of running code in the browser? What of failures in compilation? These are all incredibly annoying. The transpilation is too shit. No matter how good Smalltalk is -- it's the wrong language for Mobile. The distance to running JS is too high. The only problem at all with running JS directly on Mobile ... is how poorly things perform on the emulator, so that it's hard to develop UI and application code, even if we wanted to. So that's out; the development process is too shit. This seems to necessarily mean that we ought to program in JS, to get all the garbage out of the way. Sure, there are absolutely massive pitfalls. But why? We want strong typing that we can verify with testing.
  Why prefer JS? Because it's more portable than other languages, because browsers are portable. For that reason, development on it is more portable. All we want ... when developing, is to write verified, useful code. That's it. That's all. This gets far more uncomfortable when writing async code, and when writing code with lots of properties, because the properties are poorly documented, but are also just data. And because unlike Smalltalk, the code is not live -- the editor is unable to give information about the code without explicit types. Which is yet another point in favor of Smalltalk. What I would prefer, honestly, is to develop in Smalltalk -- but writing in JS still ends up being impossible. I don't want to export an app. What I WANT is to compile a file. And what I want in async is to write code within a single object, such that methods share state asyncronously. Thus, at any time, we can cancel a computation by referring to the shared state, rather than any monad; any async computation thus has a natural 'cancel' function that comes from the shared state.
  But short of live-edit, we rely on the static compiler to provide feedback ... and that requires a compiler. PharoJS is the wrong idea. It introduces too much cruft, and keeps the code too far from running directly in JS. It should just compile to JS. Methods should be named directly with the right name, so that they are easy to call from JS. And instance variables should be prefixed 'pj_', since they're supposed to be private anyway. We should avoid direct dependence on the Pharo platform entirely. We should avoid 'null' as well; it is NOT the same as 'nil'. It should be the responsibility of foreign libraries to hide these types if they might occur. Pharo collections that will be transpiled should NOT be different from their counterparts in JS. But this fails for strings. Strings in Smalltalk, and other privitives, have to be transpiled directly to have any chance of working.
  How much do we rely on code that can expose its own functions directly? Only really true with arrays. But the tooling seems to necessarily be shit. Clojure and clojurescript might be the only reasonable to express things while using macros to shorten the code we write. We can still express async code with core.async; we can still hide dependencies with maps (rather than interfaces).
  
What is this phenomenon that I'm experiencing? Is it the distance between writing and doing?

-}