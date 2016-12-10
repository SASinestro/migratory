module Database.Migratory.Migrate where

import Data.Typeable
import Data.Bifunctor

import Database.Migratory.Schema.Types

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
        toColTyCon "SerialCol"     = SerialColCon
        toColTyCon "IntegerCol"    = IntegerColCon
        toColTyCon "BigIntCol"     = BigIntColCon
        toColTyCon "BooleanCol"    = BooleanColCon
        toColTyCon "TextCol"       = TextColCon
        toColTyCon "JSONBCol"      = JSONBColCon
        toColTyCon "MACAddressCol" = MACAddressColCon
        toColTyCon "IPAddressCol"  = IPAddressColCon
        toColTyCon a               = error $ "Unknown type \"" ++ a ++ "\""

        toColConCon :: TypeRep -> ColumnConstraintCon
        toColConCon = toColConCon' . first tyConName . splitTyConApp
            where
                toColConCon' ("Unique", [])            = UniqueCon
                toColConCon' ("Nullable", [])          = NullableCon
                toColConCon' ("PrimaryKey", [])        = PrimaryKeyCon
                toColConCon' ("Reference", [tbl, col]) = ReferenceCon (symbolRepToString tbl) (symbolRepToString col)
                toColConCon' (a, _)                    = error $ "Unknown constraint \"" ++ a ++ "\""

        toColumn = (\[name, ty, cons] -> ColumnCon (symbolRepToString name) (toColTyCon . tyConName . typeRepTyCon $ ty) (tylmap toColConCon cons)) . typeRepArgs

        toTable = (\[name, cols] -> TableCon (symbolRepToString name) (tylmap toColumn cols)) . typeRepArgs
        tbls = tylmap toTable . head . typeRepArgs $ typeOf db
