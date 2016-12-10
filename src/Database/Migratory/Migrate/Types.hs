module Database.Migratory.Migrate.Types (Migration(..), migrationStateVector) where

import Data.Kind
import Data.Typeable

import Database.Migratory.Schema
import Database.Migratory.Schema.ToValue

data Migration :: [Type] -> [Type] -> Type where
    RootMigration :: (tbls ~ '[]) =>
        { _migrationId :: Int
        , _migrationName :: String
        , _migrationAction :: DatabaseDef tbls tbls' ()
        } -> Migration tbls tbls'
    Migration :: forall tbls tbls' tbls''.
        { _migrationId :: Int
        , _migrationName :: String
        , _parent :: Migration tbls tbls'
        , _migrationAction :: DatabaseDef tbls' tbls'' ()
        } -> Migration tbls' tbls''

data MigrationCon = MigrationCon { _mconId :: Int, _mconName :: String, _mconPreviousState :: DatabaseCon, _mconCurrentState :: DatabaseCon }

migrationStateVector :: forall tbls tbls' . (Typeable tbls, Typeable tbls') => Migration tbls tbls' -> (DatabaseState, DatabaseState)
migrationStateVector _ = (toDatabaseState (Database :: Database tbls), toDatabaseState (Database :: Database tbls'))


