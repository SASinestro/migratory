module Database.Migratory.Schema
    ( ColumnConstraint(Unique, Nullable, PrimaryKey)
    , ColumnType(..)
    , ColumnName(..)
    , Column(..)
    , ColumnNamed
    , TableName(..)
    , Table(..)
    , TableDef
    , DatabaseDef
    , addColumn
    , alterColumn
    , dropColumn
    , addForeignKeyColumn
    , addForeignKeyToSelfColumn
    , addForeignKeyToColumn
    , addForeignKeyToSelfToColumn
    , dropForeignKeyFromColumn
    , dropForeignKeyToSelfFromColumn
    , mkTable
    , updateTable
    , addTable
    , alterTable
    , dropTable
    , mkDatabase
    , updateDatabase
    ) where

import Control.Monad.Indexed
import Control.Monad.Indexed.State
import GHC.TypeLits

import Database.Migratory.Schema.TypeFunctions
import Database.Migratory.Schema.Types

--

colBody :: (IxMonadState m, KnownSymbol tblname, KnownSymbol name) => m (Table tblname i) (Table tblname j) (Column name ty cons)
colBody = iput Table >>>= \_ -> ireturn Column

type AddColumn name ty cons = forall tblname col . (KnownSymbol name, NoColumnNamed name col) => TableDef tblname col (Column name ty cons:col) (Column name ty cons)
addColumn :: Column name ty cons -> AddColumn name ty cons
addColumn _ = colBody

type AlterColumn name ty cons = forall tblname col . (KnownSymbol name, HasColumnNamed name col) => TableDef tblname col (UpdateColumn (Column name ty cons) col) (Column name ty cons)
alterColumn :: Column name ty cons -> AlterColumn name ty cons
alterColumn _ = colBody

type DropColumn name ty cons = forall tblname col . (KnownSymbol name, HasColumn (Column name ty cons) col) => TableDef tblname col (DropFirst (Column name ty cons) col) (Column name ty cons)
dropColumn :: Column name ty cons -> DropColumn name ty cons
dropColumn _ = colBody

addForeignKeyColumn :: (KnownSymbol name, NoColumnNamed name col, HasColumnNamed targetCol targetCols)
                    => Table targetName targetCols
                    -> ColumnName targetCol
                    -> Column name ty cons
                    -> TableDef tblname col (Column name ty (Reference targetName targetCol:cons):col)
                                               (Column name ty (Reference targetName targetCol:cons))
addForeignKeyColumn _ _ _ = iput (Table) >>>= \_ -> ireturn Column

addForeignKeyToSelfColumn :: (KnownSymbol name, NoColumnNamed name col, HasColumnNamed targetCol col)
                          => ColumnName targetCol
                          -> Column name ty cons
                          -> TableDef tblname col (Column name ty (Reference tblname targetCol:cons):col)
                                                     (Column name ty (Reference tblname targetCol:cons))
addForeignKeyToSelfColumn targetCol newCol = iget >>>= \tbl -> addForeignKeyColumn tbl targetCol newCol

addForeignKeyToColumn :: (KnownSymbol name, HasColumn (Column name ty cons) col, HasColumnNamed targetCol targetCols)
                      => Table targetName targetCols
                      -> ColumnName targetCol
                      -> Column name ty cons
                      -> TableDef tblname col (UpdateColumn (Column name ty (Reference targetName targetCol:cons)) col)
                                                               (Column name ty (Reference targetName targetCol:cons))
addForeignKeyToColumn _ _ _ = iput (Table) >>>= \_ -> ireturn Column

addForeignKeyToSelfToColumn :: (KnownSymbol name, HasColumn (Column name ty cons) col, HasColumnNamed targetCol col)
                            => ColumnName targetCol
                            -> Column name ty cons
                            -> TableDef tblname col (UpdateColumn (Column name ty (Reference tblname targetCol:cons)) col)
                                                                     (Column name ty (Reference tblname targetCol:cons))
addForeignKeyToSelfToColumn targetCol myCol = iget >>>= \tbl -> addForeignKeyToColumn tbl targetCol myCol

dropForeignKeyFromColumn :: (KnownSymbol name, HasColumn (Column name ty cons) col, HasColumnNamed targetCol targetCols)
                           => Table targetName targetCols
                           -> ColumnName targetCol
                           -> Column name ty cons
                           -> TableDef tblname col (UpdateColumn (Column name ty (DropFirst (Reference targetName targetCol) cons)) col)
                                                                    (Column name ty (DropFirst (Reference targetName targetCol) cons))
dropForeignKeyFromColumn _ _ _ = iput (Table) >>>= \_ -> ireturn Column

dropForeignKeyToSelfFromColumn :: (KnownSymbol name, HasColumn (Column name ty cons) col, HasColumnNamed targetCol col)
                                 => ColumnName targetCol
                                 -> Column name ty cons
                                 -> TableDef tblname col (UpdateColumn (Column name ty (DropFirst (Reference tblname targetCol) cons)) col)
                                                                          (Column name ty (DropFirst (Reference tblname targetCol) cons))
dropForeignKeyToSelfFromColumn targetCol myCol = iget >>>= \tbl -> dropForeignKeyFromColumn tbl targetCol myCol

--

mkTable :: (KnownSymbol name) => TableName name -> TableDef name '[] col a -> Table name col
mkTable _ defAction = snd $ runIxState defAction Table

updateTable :: (KnownSymbol name) => Table name col -> TableDef name col col' a -> Table name col'
updateTable _ defAction = snd $ runIxState defAction Table

--

tblBody :: (IxMonadState m, KnownSymbol name) => m (Database i) (Database j) (Table name cols)
tblBody = iput Database >>>= \_ -> ireturn Table

type AddTable name cols = forall tbls . (KnownSymbol name, NoTableNamed name tbls) => DatabaseDef tbls (Table name cols:tbls) (Table name cols)
addTable :: KnownSymbol name => TableName name -> TableDef name '[] tbls a -> AddTable name cols
addTable _ _ = tblBody

alterTable :: KnownSymbol name => Table name cols -> TableDef name cols cols' a -> DatabaseDef tbls (UpdateTable (Table name cols) tbls) (Table name cols)
alterTable _ _ = tblBody

type DropTable name cols = forall tbls . (KnownSymbol name, HasTable (Table name cols) tbls) => DatabaseDef tbls (DropFirst (Table name cols) tbls) (Table name cols)
dropTable :: KnownSymbol name => Table name cols -> DropTable name cols
dropTable _ = tblBody

--

mkDatabase :: DatabaseDef '[] tbls a -> Database tbls
mkDatabase defAction = snd $ runIxState defAction Database

updateDatabase :: Database tbls -> DatabaseDef tbls tbls' a -> Database tbls'
updateDatabase _ defAction = snd $ runIxState defAction Database
