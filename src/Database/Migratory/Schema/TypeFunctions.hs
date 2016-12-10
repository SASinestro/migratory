module Database.Migratory.Schema.TypeFunctions where

import Data.Kind
import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits

import Database.Migratory.Schema.Types

-- Equality:

type family EqColumnConstraint a b where
    EqColumnConstraint Unique Unique = True
    EqColumnConstraint Nullable Nullable = True
    EqColumnConstraint PrimaryKey PrimaryKey = True
    EqColumnConstraint (Reference a b) (Reference c d) = True
    EqColumnConstraint a b = False

type instance a == b = EqColumnConstraint a b

--

type family EqColumnType a b where
    EqColumnType SerialCol SerialCol = True
    EqColumnType IntegerCol IntegerCol = True
    EqColumnType BigIntCol BigIntCol = True
    EqColumnType BooleanCol BooleanCol = True
    EqColumnType TextCol TextCol = True
    EqColumnType JSONBCol JSONBCol = True
    EqColumnType MACAddressCol MACAddressCol = True
    EqColumnType IPAddressCol IPAddressCol = True
    EqColumnType a b = False

type instance a == b = EqColumnType a b

-- Table contents checking:

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

type family NoColumnNamed (name :: Symbol) (cols :: [Type]) :: Constraint where
    NoColumnNamed name ('[] :: [Type]) = Found
    NoColumnNamed name (Column name' ty' cons' : cols) =
        If (name == name')
            (TypeError (Text "A column named " :<>: ShowType name :<>: Text " already exists in the table!"))
            (NoColumnNamed name cols)

-- Database contents checking:

type family HasTable col (tbls :: [Type]) :: Constraint where
    HasTable (Table name cols) '[] =
        TypeError (Text "No table " :<>: ShowType (Table name cols) :<>: Text " exists in the database!")
    HasTable (Table name cols) (Table name' cols' : tbls) =
        If (name == name' && cols == cols')
            Found
            (HasTable (Table name cols) tbls)

type family HasTableNamed (colName :: Symbol) (tbls :: [Type]) :: Constraint where
    HasTableNamed name '[] =
        TypeError (Text "No table named " :<>: ShowType name :<>: Text " exists in the database!")
    HasTableNamed name (Table name' cols' : tbls) =
        If (name == name')
            Found
            (HasTableNamed name tbls)

type family NoTableNamed (name :: Symbol) (tbls :: [Type]) :: Constraint where
    NoTableNamed name ('[] :: [Type]) = Found
    NoTableNamed name (Table name' cols' : tbls) =
        If (name == name')
            (TypeError (Text "A Table named " :<>: ShowType name :<>: Text " already exists in the database!"))
            (NoTableNamed name tbls)

-- Lookup:

type family ColumnNamed (name :: Symbol) cols where
    ColumnNamed name (Table tbl '[]) =
        TypeError (Text "No column named " :<>: ShowType name :<>: Text " exists in the table "
            :<>: ShowType tbl :<>: Text "!")
    ColumnNamed name (Table tbl (Column name' ty' cons' : cols)) =
        If (name == name')
            (Column name ty' cons') -- Seems counterintuitive, but keeps the KnownSymbol instance at hand for GHC.
            (ColumnNamed name (Table tbl cols))

type family TableNamed (name :: Symbol) tbls where
    TableNamed name (Database '[]) =
        TypeError (Text "No table named " :<>: ShowType name :<>: Text " exists in the database!")
    TableNamed name (Database (Table name' cols' : tbls)) =
        If (name == name')
            (Table name cols') -- Seems counterintuitive, but keeps the KnownSymbol instance at hand for GHC.
            (ColumnNamed name (Database tbls))

-- Updating:

type family UpdateColumn i (ts :: [Type]) :: [Type] where
     UpdateColumn i '[] =
        TypeError (Text "Can't update column \"" :<>: ShowType i
            :<>: Text "\" in a table that doesn't have a column with that name!")
     UpdateColumn (Column name ty cons) (Column name' ty' cons' : cols) =
        If (name == name')
            (Column name ty cons : cols)
            (Column name' ty' cons' : UpdateColumn (Column name ty cons) cols)

type family UpdateTable i (ts :: [Type]) :: [Type] where
     UpdateTable i '[] =
        TypeError (Text "Can't update table \"" :<>: ShowType i
            :<>: Text "\" in a database that doesn't have a table with that name!")
     UpdateTable (Table name cols) (Table name' cols' : tbls) =
        If (name == name')
            (Table name cols : tbls)
            (Table name' cols' : UpdateColumn (Table name cols) tbls)

-- General use type function:

type family DropFirst i (ts :: [k]) :: [k] where
    DropFirst i '[] = '[]
    DropFirst i (t:ts) = If (i == t) ts (t : DropFirst i ts)
