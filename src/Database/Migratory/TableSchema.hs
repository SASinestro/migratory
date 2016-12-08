module Database.Migratory.TableSchema
    ( ColumnConstraint(Unique, Nullable, PrimaryKey)
    , ColumnType(..)
    , ColumnName(..)
    , Column(..)
    , ColumnNamed
    , TableName(..)
    , Table(..)
    , TableDefT
    , TableDef
    , addColumn
    , alterColumn
    , removeColumn
    , addForeignKeyColumn
    , addForeignKeyToSelfColumn
    , addForeignKeyToColumn
    , addForeignKeyToSelfToColumn
    , removeForeignKeyFromColumn
    , removeForeignKeyToSelfFromColumn
    , mkTableT
    , mkTable
    , updateTable
    , updateTableT
    ) where

import           Control.Monad.Indexed
import           Control.Monad.Indexed.State
import           Data.Aeson.Types
import           Data.Default
import           Data.Functor.Identity
import           Data.Int
import           Data.Kind
import           Data.Proxy
import qualified Data.Text                   as T
import           Data.Type.Bool
import           Data.Type.Equality
import           GHC.TypeLits
import           GHC.TypeLits.Symbols

data ColumnConstraint where
    Unique :: ColumnConstraint
    Nullable :: ColumnConstraint
    PrimaryKey :: ColumnConstraint
    Reference :: Symbol -> Symbol -> ColumnConstraint

type family EqColumnConstraint a b where
    EqColumnConstraint Unique Unique = True
    EqColumnConstraint Nullable Nullable = True
    EqColumnConstraint PrimaryKey PrimaryKey = True
    EqColumnConstraint (Reference a b) (Reference c d) = True
    EqColumnConstraint a b = False

type instance a == b = EqColumnConstraint a b


data ColumnType = SerialCol
                | IntCol
                | BigIntCol
                | BooleanCol
                | TextCol
                | JSONBCol
                | MACAddressCol
                | IPAddressCol
                deriving (Show, Eq)

type family EqColumnType a b where
    EqColumnType SerialCol SerialCol = True
    EqColumnType IntCol IntCol = True
    EqColumnType BigIntCol BigIntCol = True
    EqColumnType BooleanCol BooleanCol = True
    EqColumnType TextCol TextCol = True
    EqColumnType JSONBCol JSONBCol = True
    EqColumnType MACAddressCol MACAddressCol = True
    EqColumnType IPAddressCol IPAddressCol = True
    EqColumnType a b = False

type instance a == b = EqColumnType a b

type family ColumnValueType col where
    ColumnValueType (Column name SerialCol cons)     = Int32
    ColumnValueType (Column name IntCol cons)        = Int32
    ColumnValueType (Column name BigIntCol cons)     = Int64
    ColumnValueType (Column name BooleanCol cons)    = Bool
    ColumnValueType (Column name TextCol cons)       = T.Text
    ColumnValueType (Column name JSONBCol cons)      = Value
    ColumnValueType (Column name MACAddressCol cons) = T.Text -- TODO: Replace with stricter types!
    ColumnValueType (Column name IPAddressCol cons)  = T.Text

type family HumanName (v :: k) :: Symbol where
    HumanName SerialCol = "SerialCol"
    HumanName IntCol = "IntCol"
    HumanName BigIntCol = "BigIntCol"
    HumanName BooleanCol = "BooleanCol"
    HumanName TextCol = "TextCol"
    HumanName JSONBCol = "JSONBCol"
    HumanName MACAddressCol = "MACAddressCol"
    HumanName IPAddressCol = "IPAddressCol"
    HumanName Unique = "Unique"
    HumanName Nullable = "Nullable"
    HumanName PrimaryKey = "PrimaryKey"
    HumanName (Reference a b) = "Reference"
    HumanName '[] = ""
    HumanName '[x] = HumanName x
    HumanName (x:xs) = HumanName x +++ ", " +++ HumanName xs
    HumanName (Column name ty cons) = "Column (name = \"" +++ name +++ "\", type = " +++ HumanName ty +++ ", constraints = [" +++ HumanName cons +++ "])"
    HumanName (Table name cols) = "Table (name = \"" +++ name +++ "\", columns: [" +++ HumanName cols +++ "])"

--

data ColumnName :: Symbol -> Type where
    ColumnName :: KnownSymbol sym => ColumnName sym

instance KnownSymbol sym => Show (ColumnName sym) where
    show = symbolVal

instance KnownSymbol name => Default (ColumnName name) where
    def = ColumnName

data Column :: Symbol -> ColumnType -> [ColumnConstraint] -> Type where
    Column :: KnownSymbol name => Column name type' constraints
    WithDefault :: KnownSymbol name => ColumnValueType (Column name ty cons) -> Column name ty cons

instance KnownSymbol (HumanName (Column name type' constraints)) => Show (Column name type' constraints) where
    show _ = symbolVal (Proxy :: Proxy (HumanName (Column name type' constraints)))

instance KnownSymbol name => Default (Column name ty cons) where
    def = Column

--

data TableName :: Symbol -> Type where
    TableName :: KnownSymbol sym => TableName sym

instance KnownSymbol sym => Show (TableName sym) where
    show = symbolVal

instance KnownSymbol sym => Default (TableName sym) where
    def = TableName

data Table :: Symbol -> [Type] -> Type where
    Table :: KnownSymbol name => Table name cols

instance KnownSymbol (HumanName (Table name cols)) => Show (Table name cols) where
    show _ = symbolVal (Proxy :: Proxy (HumanName (Table name cols)))

instance KnownSymbol name => Default (Table name cols) where
    def = Table

type Found = True ~ True

type family HasColumn col (cols :: [Type]) :: Constraint where
    HasColumn (Column name ty cons) '[] =
        TypeError (Text "No column " :<>: ShowType (Column name ty cons) :<>: Text " exists in the table!")
    HasColumn (Column name ty cons) (Column name' ty' cons' : cols) =
        If (name == name' && ty == ty' && cons == cons')
            Found
            (HasColumn (Column name ty cons) cols)

type family HasColumnNamed (colName :: Symbol) (cols :: [Type]) :: Constraint where
    HasColumnNamed name '[] =
        TypeError (Text "No column named " :<>: ShowType name :<>: Text " exists in the table!")
    HasColumnNamed name (Column name' ty' cons' : cols) =
        If (name == name')
            Found
            (HasColumnNamed name cols)

type family ColumnNamed (name :: Symbol) cols where
    ColumnNamed name (Table tbl '[]) =
        TypeError (Text "No column named " :<>: ShowType name :<>: Text " exists in the table "
            :<>: ShowType tbl :<>: Text "!")
    ColumnNamed name (Table tbl (Column name' ty' cons' : cols)) =
        If (name == name')
            (Column name' ty' cons')
            (ColumnNamed name (Table tbl cols))

type family UniquelyNamed (name :: Symbol) (cols :: [Type]) :: Constraint where
    UniquelyNamed name ('[] :: [Type]) = Found
    UniquelyNamed name (Column name' ty' cons' : cols) =
        If (name == name')
            (TypeError (Text "A column named " :<>: ShowType name :<>: Text " already exists in the table!"))
            (UniquelyNamed name cols)

type family UpdateColumn i (ts :: [Type]) :: [Type] where
     UpdateColumn i '[] =
        TypeError (Text "Can't update column \"" :<>: ShowType i
            :<>: Text "\" in a table that doesn't have a column with that name!")
     UpdateColumn (Column name ty cons) (Column name' ty' cons' : ts) =
        If (name == name')
            (Column name ty cons : ts)
            (Column name' ty' cons' : UpdateColumn (Column name ty cons) ts)

type family RemoveFirst i (ts :: [k]) :: [k] where
    RemoveFirst i '[] = '[]
    RemoveFirst i (t:ts) = If (i == t) ts (t : RemoveFirst i ts)

--

type TableDefT m name col col' a = KnownSymbol name => IxStateT m (Table name col) (Table name col') a
type TableDef name col col' a = TableDefT Identity name col col' a

addColumn :: (Monad m, UniquelyNamed name col)
          => Column name ty cons
          -> TableDefT m tblname col (Column name ty cons:col) (Column name ty cons)
addColumn col = iput (Table) >>>= \_ -> ireturn col

alterColumn :: (Monad m, HasColumnNamed name col)
            => Column name ty cons
            -> TableDefT m tblname col (UpdateColumn (Column name ty cons) col) (Column name ty cons)
alterColumn col = iput (Table) >>>= \_ -> ireturn col

removeColumn :: (Monad m, HasColumn (Column name ty cons) col)
             => Column name ty cons
             -> TableDefT m tblname col (RemoveFirst (Column name ty cons) col) ()
removeColumn _ = iput (Table)

addForeignKeyColumn :: (Monad m, KnownSymbol name, UniquelyNamed name col, HasColumnNamed targetCol targetCols)
                    => Table targetName targetCols
                    -> ColumnName targetCol
                    -> Column name ty cons
                    -> TableDefT m tblname col (Column name ty (Reference targetName targetCol:cons):col)
                                               (Column name ty (Reference targetName targetCol:cons))
addForeignKeyColumn _ _ _ = iput (Table) >>>= \_ -> ireturn Column

addForeignKeyToSelfColumn :: (Monad m, KnownSymbol name, UniquelyNamed name col, HasColumnNamed targetCol col)
                          => ColumnName targetCol
                          -> Column name ty cons
                          -> TableDefT m tblname col (Column name ty (Reference tblname targetCol:cons):col)
                                                     (Column name ty (Reference tblname targetCol:cons))
addForeignKeyToSelfColumn targetCol newCol = iget >>>= \tbl -> addForeignKeyColumn tbl targetCol newCol

addForeignKeyToColumn :: (Monad m, KnownSymbol name, HasColumn (Column name ty cons) col, HasColumnNamed targetCol targetCols)
                      => Table targetName targetCols
                      -> ColumnName targetCol
                      -> Column name ty cons
                      -> TableDefT m tblname col (UpdateColumn (Column name ty (Reference targetName targetCol:cons)) col)
                                                               (Column name ty (Reference targetName targetCol:cons))
addForeignKeyToColumn _ _ _ = iput (Table) >>>= \_ -> ireturn Column

addForeignKeyToSelfToColumn :: (Monad m, KnownSymbol name, HasColumn (Column name ty cons) col, HasColumnNamed targetCol col)
                            => ColumnName targetCol
                            -> Column name ty cons
                            -> TableDefT m tblname col (UpdateColumn (Column name ty (Reference tblname targetCol:cons)) col)
                                                                     (Column name ty (Reference tblname targetCol:cons))
addForeignKeyToSelfToColumn targetCol myCol = iget >>>= \tbl -> addForeignKeyToColumn tbl targetCol myCol

removeForeignKeyFromColumn :: (Monad m, KnownSymbol name, HasColumn (Column name ty cons) col, HasColumnNamed targetCol targetCols)
                           => Table targetName targetCols
                           -> ColumnName targetCol
                           -> Column name ty cons
                           -> TableDefT m tblname col (UpdateColumn (Column name ty (RemoveFirst (Reference targetName targetCol) cons)) col)
                                                                    (Column name ty (RemoveFirst (Reference targetName targetCol) cons))
removeForeignKeyFromColumn _ _ _ = iput (Table) >>>= \_ -> ireturn Column

removeForeignKeyToSelfFromColumn :: (Monad m, KnownSymbol name, HasColumn (Column name ty cons) col, HasColumnNamed targetCol col)
                                 => ColumnName targetCol
                                 -> Column name ty cons
                                 -> TableDefT m tblname col (UpdateColumn (Column name ty (RemoveFirst (Reference tblname targetCol) cons)) col)
                                                                          (Column name ty (RemoveFirst (Reference tblname targetCol) cons))
removeForeignKeyToSelfFromColumn targetCol myCol = iget >>>= \tbl -> removeForeignKeyFromColumn tbl targetCol myCol

--

mkTableT :: (Monad m, KnownSymbol name) => TableName name -> TableDefT m name '[] col a -> m (Table name col)
mkTableT _ defAction = snd <$> runIxStateT defAction Table

mkTable :: (KnownSymbol name) => TableName name -> TableDef name '[] col a -> Table name col
mkTable _ defAction = runIdentity $ snd <$> runIxStateT defAction Table

updateTableT :: (Monad m, KnownSymbol name) => Table name col -> TableDefT m name col col' a -> m (Table name col')
updateTableT _ defAction = snd <$> runIxStateT defAction Table

updateTable :: (KnownSymbol name) => Table name col -> TableDef name col col' a -> Table name col'
updateTable _ defAction = runIdentity $ snd <$> runIxStateT defAction Table
