namespace Examples.SQLite
open System
open System.Collections.Generic
open System.Globalization

type Literal =
    | NullLiteral
    | CurrentTimeLiteral
    | CurrentDateLiteral
    | CurrentTimestampLiteral
    | StringLiteral of string
    | BlobLiteral of byte array
    | IntegerLiteral of int64
    | FloatLiteral of float

type Name = string
    
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
    | Like
    | Glob
    | Match
    | Regexp
    | And
    | Or

type UnaryOperator =
    | Negative
    | Not
    | BitNot

type Expr =
    | LiteralExpr of Literal
    | BindParameterExpr of Name
    | FunctionInvocationExpr of FunctionInvocationExpr
    | BinaryExpr of BinaryOperator * Expr * Expr
    | UnaryExpr of UnaryOperator * Expr
    | BetweenExpr of Expr * Expr * Expr
    | NotBetweenExpr of Expr * Expr * Expr
    | InExpr of Expr * InSet
    | NotInExpr of Expr * InSet

and FunctionInvocationExpr =
    {
        FunctionName : Name
        Distinct : bool
        Arguments : FunctionArguments
    }

and FunctionArguments =
    | ArgumentWildcard
    | ArgumentList of Expr ResizeArray

and InSet =
    | InExpressions of Expr ResizeArray
    | InSelect of SelectStmt

and SelectStmt =
    | SelectStmt