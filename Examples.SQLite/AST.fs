namespace Examples.SQLite
open System
open System.Collections.Generic
open System.Globalization

type NumericLiteral =
    | IntegerLiteral of int64
    | FloatLiteral of float

type SignedNumericLiteral =
    {
        Sign : int // -1, 0, 1
        Value : NumericLiteral
    }
    member this.ToNumericLiteral() =
        if this.Sign < 0 then
            match this.Value with
            | IntegerLiteral i -> IntegerLiteral (-i)
            | FloatLiteral f -> FloatLiteral (-f)
        else this.Value

type Literal =
    | NullLiteral
    | CurrentTimeLiteral
    | CurrentDateLiteral
    | CurrentTimestampLiteral
    | StringLiteral of string
    | BlobLiteral of byte array
    | NumericLiteral of NumericLiteral

type Name = string

type Alias = Name option

type TypeBounds =
    {
        Low : SignedNumericLiteral
        High : SignedNumericLiteral option
    }

type TypeName =
    {
        TypeName : Name list
        Bounds : TypeBounds option
    }

type TableName =
    {
        SchemaName : Name option
        TableName : Name
    }

type ColumnName =
    {
        Table : TableName option
        ColumnName : Name
    }

type BindParameter =
    | NamedParameter of char * string // char is the prefix: ':', '@', or '$'
    | PositionalParameter of uint32 option
    
type BinaryOperator =
    | Concatenate
    | Multiply
    | Divide
    | Modulo
    | Add
    | Subtract
    | BitShiftLeft
    | BitShiftRight
    | BitAnd
    | BitOr
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | Equal
    | NotEqual
    | Is
    | IsNot
    | And
    | Or

type UnaryOperator =
    | Negative
    | Not
    | BitNot

type SimilarityOperator =
    | Like
    | Glob
    | Match
    | Regexp

type Expr =
    | LiteralExpr of Literal
    | BindParameterExpr of BindParameter
    | ColumnNameExpr of ColumnName
    | CastExpr of CastExpr
    | CollateExpr of Expr * Name
    | FunctionInvocationExpr of FunctionInvocationExpr
    | SimilarityExpr of SimilarityOperator * Expr * Expr * Expr option // optional ESCAPE clause
    | BinaryExpr of BinaryOperator * Expr * Expr
    | UnaryExpr of UnaryOperator * Expr
    | BetweenExpr of Expr * Expr * Expr
    | NotBetweenExpr of Expr * Expr * Expr
    | InExpr of Expr * InSet
    | NotInExpr of Expr * InSet
    | ExistsExpr of SelectStmt
    | CaseExpr of CaseExpr
    | ScalarSubqueryExpr of SelectStmt

and CastExpr =
    {
        Expression : Expr
        AsType : TypeName
    }
 
and TableInvocation =
    {
        Table : TableName
        Arguments : Expr ResizeArray option // we use an option to distinguish between schema.table and schema.table()
    }

and FunctionInvocationExpr =
    {
        FunctionName : Name
        Arguments : FunctionArguments
    }

and CaseExpr =
    {
        Input : Expr option
        Cases : (Expr * Expr) ResizeArray
        Else : Expr option
    }

and Distinct = | Distinct

and DistinctColumns =
    | DistinctColumns
    | AllColumns

and FunctionArguments =
    | ArgumentWildcard
    | ArgumentList of (Distinct option * Expr ResizeArray)

and InSet =
    | InExpressions of Expr ResizeArray
    | InSelect of SelectStmt
    | InTable of TableInvocation

and SelectStmt =
    {
        With : WithClause option
        Compound : CompoundExpr
        OrderBy : OrderingTerm ResizeArray option
        Limit : Limit option
    }

and WithClause =
    {
        Recursive : bool
        Tables : CommonTableExpression ResizeArray
    }

and CommonTableExpression =
    {
        Name : TableName
        ColumnNames : ColumnName ResizeArray option
        AsSelect : SelectStmt
    }

and OrderDirection =
    | Ascending
    | Descending

and OrderingTerm =
    {
        By : Expr
        Direction : OrderDirection
    }

and Limit =
    {
        Limit : Expr
        Offset : Expr option
    }

and CompoundExpr =
    | CompoundTerm of CompoundTerm
    | Union of CompoundExpr * CompoundTerm
    | UnionAll of CompoundExpr * CompoundTerm
    | Intersect of CompoundExpr * CompoundTerm
    | Except of CompoundExpr * CompoundTerm

and CompoundTerm =
    | Values of Expr ResizeArray ResizeArray
    | Select of SelectCore

and SelectCore =
    {
        Columns : ResultColumns
        From : TableExpr option
        Where : Expr option
        GroupBy : GroupBy option
    }

and GroupBy =
    {
        By : Expr ResizeArray
        Having : Expr option
    }

and ResultColumns =
    {
        Distinct : DistinctColumns option
        Columns : ResultColumn ResizeArray
    }

and ResultColumn =
    | ColumnsWildcard
    | TableColumnsWildcard of TableName
    | Column of Expr * Alias

and IndexHint =
    | IndexedBy of Name
    | NotIndexed

and TableOrSubquery =
    | Table of TableInvocation * Alias * IndexHint option // note: an index hint is invalid if the table has args
    | Subquery of SelectStmt * Alias

and JoinType =
    | Inner
    | LeftOuter
    | Cross
    | Natural of JoinType

and JoinConstraint =
    | JoinOn of Expr
    | JoinUsing of ColumnName list
    | JoinUnconstrained

and TableExpr =
    | TableOrSubquery of TableOrSubquery
    | AliasedTableExpr of TableExpr * Alias
    | Join of JoinType *  TableExpr * TableExpr * JoinConstraint

type ConflictClause =
    | Rollback
    | Abort
    | Fail
    | Ignore
    | Replace

type ForeignKeyEvent =
    | OnDelete
    | OnUpdate

type ForeignKeyEventHandler =
    | SetNull
    | SetDefault
    | Cascade
    | Restrict
    | NoAction

type ForeignKeyRule =
    | MatchRule of Name
    | EventRule of (ForeignKeyEvent * ForeignKeyEventHandler)

type ForeignKeyDeferClause =
    {
        Deferrable : bool
        InitiallyDeferred : bool option
    }

type ForeignKeyClause =
    {
        ReferencesTable : TableName
        ReferencesColumns : Name ResizeArray option
        Rules : ForeignKeyRule ResizeArray
        Defer : ForeignKeyDeferClause option
    }

type PrimaryKeyClause =
    {
        Order : OrderDirection
        ConflictClause : ConflictClause option
        AutoIncrement : bool
    }

type ColumnConstraintType =
    | PrimaryKeyConstraint of PrimaryKeyClause
    | NotNullConstraint of ConflictClause option
    | UniqueConstraint of ConflictClause option
    | CheckConstraint of Expr
    | DefaultConstraint of Expr
    | CollateConstraint of Name
    | ForeignKeyConstraint of ForeignKeyClause

type ColumnConstraint =
    {
        Name : Name option
        ColumnConstraintType : ColumnConstraintType
    }

type ColumnDef =
    {
        Name : Name
        Type : TypeName option
        Constraints : ColumnConstraint ResizeArray
    }

type AlterTableAlteration =
    | RenameTo of Name
    | AddColumn of ColumnDef

type AlterTableStmt =
    {
        Table : TableName
        Alteration : AlterTableAlteration
    }

type TableIndexConstraintType =
    | PrimaryKey
    | Unique

type TableIndexConstraintClause =
    {
        Type : TableIndexConstraintType
        IndexedColumns : (Expr * OrderDirection) ResizeArray
        ConflictClause : ConflictClause option
    }

type TableConstraintType =
    | TableIndexConstraint of TableIndexConstraintClause
    | TableForeignKeyConstraint of Name ResizeArray * ForeignKeyClause
    | TableCheckConstraint of Expr

type TableConstraint =
    {
        Name : Name option
        TableConstraintType : TableConstraintType
    }

type CreateTableDefinition =
    {
        Columns : ColumnDef ResizeArray
        Constraints : TableConstraint ResizeArray
        WithoutRowId : bool
    }

type CreateTableAs =
    | CreateAsDefinition of CreateTableDefinition
    | CreateAsSelect of SelectStmt

type CreateTableStmt =
    {
        Temporary : bool
        IfNotExists : bool
        Name : TableName
        As : CreateTableAs
    }

type TransactionType =
    | Deferred
    | Immediate
    | Exclusive

type SavepointName = Name

type Stmt =
    | AlterTableStmt of AlterTableStmt
    | AnalyzeStmt of TableName
    | AttachStmt of Expr * Name
    | BeginStmt of TransactionType
    | CommitStmt
    | CreateTableStmt of CreateTableStmt
    | RollbackStmt of SavepointName option
    | SelectStmt of SelectStmt
    | ExplainStmt of Stmt