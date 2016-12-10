module Database.Migratory.Schema
    ( ColumnConstraint(Unique, Nullable, PrimaryKey)
    , ColumnType(..)
    , ColumnName(..)
    , Column(..)
    , ColumnNamed
    , TableName(..)
    , Table(..)
    , TableDef
    , Database(..)
    , DatabaseDef
    , addColumn
    , alterColumn
    , dropColumn
    , getColumn
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
    , getTable
    , mkDatabase
    , updateDatabase
    ) where

import Control.Monad.Indexed
import Control.Monad.Indexed.State
import GHC.TypeLits

import Database.Migratory.Schema.TypeFunctions
import Database.Migratory.Schema.Types

--

colBody :: (IxMonadState m, KnownSymbol tblname) => m (Table tblname i) (Table tblname j) ()
colBody = iput Table

type AddColumn name ty cons = forall tblname col . (KnownSymbol name, NoColumnNamed name col) => TableDef tblname col (Column name ty cons:col) ()
addColumn :: Column name ty cons -> AddColumn name ty cons
addColumn _ = colBody

type AlterColumn name ty cons = forall tblname col . (KnownSymbol name, HasColumnNamed name col) => TableDef tblname col (UpdateColumn (Column name ty cons) col) ()
alterColumn :: Column name ty cons -> AlterColumn name ty cons
alterColumn _ = colBody

type DropColumn name ty cons = forall tblname col . (KnownSymbol name, HasColumn (Column name ty cons) col) => TableDef tblname col (DropFirst (Column name ty cons) col) ()
dropColumn :: Column name ty cons -> DropColumn name ty cons
dropColumn _ = colBody

getColumn :: forall name tblname cols ty cons . (KnownSymbol name, HasColumnNamed name cols, (ColumnNamed name (Table tblname cols)) ~ (Column name ty cons))
          => ColumnName name -> TableDef tblname cols cols (Column name ty cons)
getColumn _ = return (Column :: Column name ty cons)

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
updateTable tbl defAction = snd $ runIxState defAction tbl

--

tblBody :: (IxMonadState m) => m (Database i) (Database j) ()
tblBody = iput Database

type AddTable name cols = forall tbls . (KnownSymbol name, NoTableNamed name tbls) => DatabaseDef tbls (Table name cols:tbls) ()
addTable :: KnownSymbol name => TableName name -> TableDef name '[] cols a -> AddTable name cols
addTable _ _ = tblBody

alterTable :: KnownSymbol name => Table name cols -> TableDef name cols cols' a -> DatabaseDef tbls (UpdateTable (Table name cols) tbls) ()
alterTable _ _ = tblBody

type DropTable name cols = forall tbls . (KnownSymbol name, HasTable (Table name cols) tbls) => DatabaseDef tbls (DropFirst (Table name cols) tbls) ()
dropTable :: KnownSymbol name => Table name cols -> DropTable name cols
dropTable _ = tblBody

getTable :: (KnownSymbol name, HasTableNamed name tbls, (TableNamed name (Database tbls)) ~ (Table name cols))
         => TableName name -> DatabaseDef tbls tbls (Table name cols)
getTable _ = ireturn Table

--

mkDatabase :: DatabaseDef '[] tbls a -> Database tbls
mkDatabase defAction = snd $ runIxState defAction Database

updateDatabase :: Database tbls -> DatabaseDef tbls tbls' a -> Database tbls'
updateDatabase db defAction = snd $ runIxState defAction db
