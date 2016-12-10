module Database.Migratory.MigrationPrelude (module Prelude, module Database.Migratory.Schema, module Control.Monad.Indexed, ifThenElse, tName, col, cName) where

import           Control.Monad.Indexed
import           Database.Migratory.Schema
import           GHC.TypeLits
import           Prelude                   hiding (Monad (..))

ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _  = a
ifThenElse False b _ = b

tName :: KnownSymbol tblName => TableName tblName
tName = TableName

col :: KnownSymbol name => Column name ty cons
col = Column

cName :: KnownSymbol name => ColumnName name
cName = ColumnName
