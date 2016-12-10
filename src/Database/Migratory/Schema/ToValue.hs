{-# LANGUAGE OverloadedStrings #-}
module Database.Migratory.Schema.ToValue (ColumnConstraintCon(..), ColumnTypeCon(..), ColumnCon(..), TableCon(..), DatabaseCon(..), toValueLevelRep, toByteStringRep, toByteStringRep', DatabaseState, toDatabaseState) where

import           Crypto.Hash
import           Data.Bifunctor
import qualified Data.ByteString.Char8           as BS
import           Data.Typeable

import           Database.Migratory.Schema.Types

data ColumnConstraintCon = UniqueCon
                         | NullableCon
                         | PrimaryKeyCon
                         | ReferenceCon String String
                         deriving (Show, Eq)

data ColumnTypeCon = SerialColCon
                   | IntegerColCon
                   | BigIntColCon
                   | BooleanColCon
                   | TextColCon
                   | JSONBColCon
                   | MACAddressColCon
                   | IPAddressColCon
                   deriving (Show, Eq)

data ColumnCon = ColumnCon { _colName :: String, _colType :: ColumnTypeCon, _colConstraints :: [ColumnConstraintCon] }
               deriving (Eq, Show)

data TableCon = TableCon { _tblName :: String, _tblCols :: [ColumnCon] }
              deriving (Eq, Show)

data DatabaseCon = DatabaseCon [TableCon]
                 deriving (Eq)

toValueLevelRep :: Typeable tbls => Database tbls -> DatabaseCon
toValueLevelRep db = DatabaseCon tbls
    where
        symbolRepToString :: TypeRep -> String
        symbolRepToString = tail . init . tyConName . typeRepTyCon

        tylmap :: (TypeRep -> b) -> TypeRep -> [b]
        tylmap f = uncurry tylmap' . first tyConName . splitTyConApp
            where
                tylmap' "'[]" _          = []
                tylmap' "':" [val, rest] = f val : tylmap f rest
                tylmap' _ _              = error "Serious logic error in type-to-value conversion, this should never happen!"

        toColTyCon :: String -> ColumnTypeCon
        toColTyCon "'SerialCol"     = SerialColCon
        toColTyCon "'IntegerCol"    = IntegerColCon
        toColTyCon "'BigIntCol"     = BigIntColCon
        toColTyCon "'BooleanCol"    = BooleanColCon
        toColTyCon "'TextCol"       = TextColCon
        toColTyCon "'JSONBCol"      = JSONBColCon
        toColTyCon "'MACAddressCol" = MACAddressColCon
        toColTyCon "'IPAddressCol"  = IPAddressColCon
        toColTyCon a                = error $ "Unknown type \"" ++ a ++ "\""

        toColConCon :: TypeRep -> ColumnConstraintCon
        toColConCon = toColConCon' . first tyConName . splitTyConApp
            where
                toColConCon' ("'Unique", [])            = UniqueCon
                toColConCon' ("'Nullable", [])          = NullableCon
                toColConCon' ("'PrimaryKey", [])        = PrimaryKeyCon
                toColConCon' ("'Reference", [tbl, col]) = ReferenceCon (symbolRepToString tbl) (symbolRepToString col)
                toColConCon' (a, _)                     = error $ "Unknown constraint \"" ++ a ++ "\""

        toColumn = (\[name, ty, cons] -> ColumnCon (symbolRepToString name) (toColTyCon . tyConName . typeRepTyCon $ ty) (tylmap toColConCon cons)) . typeRepArgs

        toTable = (\[name, cols] -> TableCon (symbolRepToString name) (tylmap toColumn cols)) . typeRepArgs
        tbls = tylmap toTable . head . typeRepArgs $ typeOf db

toByteStringRep :: Typeable tbls => Database tbls -> BS.ByteString
toByteStringRep = toByteStringRep' . toValueLevelRep

toByteStringRep' :: DatabaseCon -> BS.ByteString
toByteStringRep' = dbRep
    where
        tab :: BS.ByteString
        tab = "    "
        dbRep (DatabaseCon tbls) = BS.concat ["Database\n", BS.concat (tblRep <$> tbls)]
        tblRep (TableCon name cols) = BS.concat [tab, "Table ", BS.pack name, "\n", BS.concat (colRep <$> cols)]
        colRep (ColumnCon name type' constraints) = BS.concat [tab, tab, "Column ", BS.pack name, " ", colTypeRep type', " (", BS.intercalate ", " . fmap colConstraintRep $ constraints, ")\n"]

        colTypeRep SerialColCon     = "Serial"
        colTypeRep IntegerColCon    = "Integer"
        colTypeRep BigIntColCon     = "BigInt"
        colTypeRep BooleanColCon    = "Boolean"
        colTypeRep TextColCon       = "Text"
        colTypeRep JSONBColCon      = "JSONB"
        colTypeRep MACAddressColCon = "MACAddress"
        colTypeRep IPAddressColCon  = "IPAddress"

        colConstraintRep UniqueCon              = "unique"
        colConstraintRep NullableCon            = "nullable"
        colConstraintRep PrimaryKeyCon          = "primary key"
        colConstraintRep (ReferenceCon tbl col) = BS.pack $ "reference to " ++ tbl ++ "." ++ col

instance Show (DatabaseCon) where
    show = BS.unpack . BS.init . toByteStringRep'

newtype DatabaseState = DatabaseState { _unDbState :: String }
                      deriving (Eq, Show)

toDatabaseState :: Typeable tbls => Database tbls -> DatabaseState
toDatabaseState = DatabaseState . show . hashWith SHA3_256 . toByteStringRep
