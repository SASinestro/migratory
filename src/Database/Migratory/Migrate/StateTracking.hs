{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Database.Migratory.Migrate.StateTracking (createMigrationReceiptTable, hasMigrationReceiptTable, MigrationReceipt(..), MigrationHistoryError(..), getMigrationReceipts, logMigration) where

import Data.Typeable
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow

import Database.Migratory.Migrate.Types
import Database.Migratory.Schema.ToValue

createMigrationReceiptTable :: Connection -> IO ()
createMigrationReceiptTable connection = execute_ connection
    [sql|CREATE TABLE migrations (
             "id" SERIAL PRIMARY KEY,
             "human_name" TEXT UNIQUE NOT NULL,
             "prev_state" TEXT NOT NULL,
             "current_state" TEXT NOT NULL
         );|] >> return ()

hasMigrationReceiptTable :: Connection -> IO Bool
hasMigrationReceiptTable connection = fromOnly . head <$> query_ connection "SELECT pg_table_is_visible(to_regclass('migrations'));"

data MigrationReceipt = MigrationReceipt {
          _mrId           :: Int
        , _mrHumanName    :: String
        , _mrPrevState    :: DatabaseState
        , _mrCurrentState :: DatabaseState
    }
    deriving (Show, Eq)

instance FromRow MigrationReceipt where
    fromRow = MigrationReceipt <$> field <*> field <*> field <*> field

instance ToRow MigrationReceipt where
    toRow MigrationReceipt{..} = [toField _mrId, toField _mrHumanName, toField _mrPrevState, toField _mrCurrentState]

data MigrationHistoryError where
    MissingMigration :: MigrationReceipt -> MigrationHistoryError
    InvalidMigration :: Migration tbls tbls' -> MigrationReceipt -> MigrationHistoryError

getMigrationReceipts :: Connection -> IO [MigrationReceipt]
getMigrationReceipts conn = query_ conn "SELECT * FROM migrations;"

logMigration :: (Typeable tbls, Typeable tbls') => Connection -> Migration tbls tbls' -> IO ()
logMigration conn m = execute conn "INSERT ? INTO migrations;" receipt >> return ()
    where
        id' = _migrationId m
        name = _migrationName m
        (prev, current) = migrationStateVector m
        receipt = MigrationReceipt id' name prev current
