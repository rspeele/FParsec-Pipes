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
    | ExistsExpr of Expr * SelectStmt
    | NotExistsExpr of Expr * SelectStmt
    | CaseExpr of CaseExpr

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
    | SelectStmt

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


and CommonTableExpression =
    {
        Name : TableName
        Recursive : bool
        ColumnNames : ColumnName ResizeArray option
        AsSelect : SelectStmt
    }