module Database.Migratory.Schema.Types
    ( TyShow
    , ColumnConstraint(..)
    , ColumnType(..)
    , ColumnValueType
    , ColumnName(..)
    , Column(..)
    , TableDef
    , TableName(..)
    , Table(..)
    , DatabaseDef
    , Database(..)
    ) where


import           Control.Monad.Indexed.State
import           Data.Aeson.Types
import           Data.Default
import           Data.Int
import           Data.Kind
import           Data.Proxy
import qualified Data.Text                   as T
import           Data.Typeable
import           GHC.TypeLits
import           GHC.TypeLits.Symbols

--

-- | A type family used as a type-level equivalent of 'show'.
type family TyShow (v :: k) :: Symbol where
    TyShow '[] = ""
    TyShow '[x] = TyShow x
    TyShow (x:xs) = TyShow x +++ ", " +++ TyShow xs

    TyShow Unique = "Unique"
    TyShow Nullable = "Nullable"
    TyShow PrimaryKey = "PrimaryKey"
    TyShow (Reference a b) = "Reference"

    TyShow SerialCol = "SerialCol"
    TyShow IntegerCol = "IntegerCol"
    TyShow BigIntCol = "BigIntCol"
    TyShow BooleanCol = "BooleanCol"
    TyShow TextCol = "TextCol"
    TyShow JSONBCol = "JSONBCol"
    TyShow MACAddressCol = "MACAddressCol"
    TyShow IPAddressCol = "IPAddressCol"

    TyShow (Column name ty cons) =
        "Column (name = \"" +++ name +++ "\", type = " +++ TyShow ty +++ ", constraints = [" +++ TyShow cons +++ "])"

    TyShow (Table name cols) = "Table (name = \"" +++ name +++ "\", columns: [" +++ TyShow cols +++ "])"
    TyShow (Database tbls) = "Database (columns = [" +++ TyShow tbls +++ "])"

--

data ColumnConstraint = Unique
                      | Nullable
                      | PrimaryKey
                      | Reference Symbol Symbol
                      deriving (Typeable)

deriving instance Typeable ColumnConstraint


--

-- | The supported PostgreSQL data types.
data ColumnType = SerialCol     -- ^ 'serial'
                | IntegerCol    -- ^ 'integer'
                | BigIntCol     -- ^ 'bigint'
                | BooleanCol    -- ^ 'boolean'
                | TextCol       -- ^ 'text'
                | JSONBCol      -- ^ 'jsonb'
                | MACAddressCol -- ^ 'macaddr'
                | IPAddressCol  -- ^ 'inet'
                deriving (Show, Eq, Typeable)

type family ColumnValueType col where
    ColumnValueType (Column name SerialCol cons)     = Int32
    ColumnValueType (Column name IntegerCol cons)    = Int32
    ColumnValueType (Column name BigIntCol cons)     = Int64
    ColumnValueType (Column name BooleanCol cons)    = Bool
    ColumnValueType (Column name TextCol cons)       = T.Text
    ColumnValueType (Column name JSONBCol cons)      = Value
    ColumnValueType (Column name MACAddressCol cons) = T.Text -- TODO: Replace with stricter types!
    ColumnValueType (Column name IPAddressCol cons)  = T.Text

--

data ColumnName :: Symbol -> Type where
    ColumnName :: KnownSymbol sym => ColumnName sym

instance KnownSymbol sym => Show (ColumnName sym) where
    show = symbolVal

instance KnownSymbol name => Default (ColumnName name) where
    def = ColumnName

--

data Column :: Symbol -> ColumnType -> [ColumnConstraint] -> Type where
    Column :: KnownSymbol name => Column name type' constraints
    WithDefault :: KnownSymbol name => ColumnValueType (Column name ty cons) -> Column name ty cons

instance KnownSymbol name => Default (Column name ty cons) where
    def = Column

instance KnownSymbol (TyShow (Column name type' constraints)) => Show (Column name type' constraints) where
    show _ = symbolVal (Proxy :: Proxy (TyShow (Column name type' constraints)))

--

data TableName :: Symbol -> Type where
    TableName :: KnownSymbol sym => TableName sym

instance KnownSymbol sym => Show (TableName sym) where
    show = symbolVal

instance KnownSymbol sym => Default (TableName sym) where
    def = TableName

--

type TableDef name col col' a = KnownSymbol name => IxState (Table name col) (Table name col') a

data Table :: Symbol -> [Type] -> Type where
    Table :: KnownSymbol name => Table name cols

instance KnownSymbol name => Default (Table name cols) where
    def = Table

instance (KnownSymbol name, KnownSymbol (TyShow (Table name cols))) => Show (Table name cols) where
    show _ = symbolVal (Proxy :: Proxy (TyShow (Table name cols)))

--

type DatabaseDef tbl tbl' a = IxState (Database tbl) (Database tbl') a

data Database :: [Type] -> Type where
    Database :: Database tbls

instance KnownSymbol (TyShow (Database tbl)) => Show (Database tbl) where
    show _ = symbolVal (Proxy :: Proxy (TyShow (Database tbl)))

instance Default (Database cols) where
    def = Database

deriving instance Typeable (Database tbls)
