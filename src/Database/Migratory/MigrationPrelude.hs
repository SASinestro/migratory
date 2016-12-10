module Database.Migratory.MigrationPrelude (module Prelude, module Database.Migratory.Schema, module Control.Monad.Indexed, ifThenElse, return, (>>=), (>>), tName, col, cName) where

import           Control.Monad.Indexed
import           Database.Migratory.Schema
import           GHC.TypeLits
import           Prelude                   hiding (Monad (..))

ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _  = a
ifThenElse False b _ = b

return :: forall a i m. (IxMonad m) =>  a -> m i i a
return = ireturn

(>>=) :: forall a b i j k m. (IxMonad m) => m i j a -> (a -> m j k b) -> m i k b
(>>=) = (>>>=)

(>>) :: forall a b i j k m. (IxMonad m) => m i j a -> m j k b -> m i k b
(>>) = \a b -> a >>>= (\_ -> b)

tName :: KnownSymbol tblName => TableName tblName
tName = TableName

col :: KnownSymbol name => Column name ty cons
col = Column

cName :: KnownSymbol name => ColumnName name
cName = ColumnName
