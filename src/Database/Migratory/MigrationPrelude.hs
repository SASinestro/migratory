module Database.Migratory.MigrationPrelude (module Prelude, module Database.Migratory.TableSchema, module Control.Monad.Indexed, (>>>), ifThenElse) where

import Control.Monad.Indexed
import Database.Migratory.Schema
import Prelude                        hiding (Monad (..))

(>>>) :: forall a b i j k m. IxMonad m => m i j a -> m j k b -> m i k b
(>>>) = \a b -> a >>>= (\_ -> b)

ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _  = a
ifThenElse False b _ = b
