module Webb.Sql.Query.Analyze.BuildTables where

import Prelude
import Webb.Sql.Query.Analyze.AnalyzeM
import Webb.Sql.Query.Analyze.Types

import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String as Str
import Effect.Class (class MonadEffect)
import Webb.Random (randomId)
import Webb.Sql.Query.Parser as P
import Webb.State.Prelude (mread)
import Webb.Stateful.MapColl as M


{- Analyze the query tree for aliases and local tables. 

-}

addAllTables :: forall m. MonadEffect m => Analyze m Boolean
addAllTables = do
  this <- mread
  addAliases this.tree.from
  isSuccess


class AddTables a where
  addAliases :: forall m. MonadEffect m => a -> Analyze m Unit
  
instance AddTables P.From where
  addAliases from = case from of
    P.Tables tables -> do
      for_ tables addAliases
    P.Joins join -> do 
      addAliases join
      
instance AddTables P.TableData where
  addAliases self = do
    this <- mread
    let td = unwrap self
    let table = td.table.string
        malias = _.string <$> td.alias
        name = Str.toLower $ fromMaybe table malias :: String
    confirmAlias name
    M.insert this.aliases name table
    
instance AddTables P.Join where
  addAliases join = case join of
    P.Table td -> addAliases td 
    P.JoinOp { table1, table2 } -> do
      addAliases table1
      addAliases table2
    P.SubQuery { query, alias: token } -> do 
      -- We don't add the alias here, but later, as part of building the table.
      this <- mread
      case token of
        Nothing -> do
          warn "A subquery must provide an alias"
        Just alias -> do
          -- Insert the alias with a random id.
          let name = alias.string
          confirmAlias name
          tableId <- randomId
          M.insert this.aliases name tableId

          -- Add a local table for the query to refer to.
          let tableDef = analyzeQuery this.ex query  
          M.insert this.locals tableId tableDef

confirmAlias :: forall m. MonadEffect m => String -> AnalyzeM m Unit
confirmAlias alias = do
  this <- mread
  whenM (M.member this.aliases alias) do
    warn $ "The table alias is already used: " <> alias
