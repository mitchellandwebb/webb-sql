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


{-
buildThis :: forall m. MonadEffect m => Analyze m Boolean
buildThis = do
  checkSelect
  checkFrom
  checkWhere
  checkGroupBy
  checkOrderBy

  isSuccess
  
  where
  checkSelect = pure unit
  checkFrom = pure unit
  checkWhere = pure unit
  checkGroupBy = pure unit
  checkOrderBy = pure unit
 -} 