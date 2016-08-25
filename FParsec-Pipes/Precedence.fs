// The MIT License (MIT)
// Copyright (c) 2016 Robert Peele
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software
// and associated documentation files (the "Software"), to deal in the Software without restriction,
// including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
// BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

[<AutoOpen>]
module FParsec.Pipes.Precedence
open FParsec

type Associativity =
    | LeftAssociative
    | RightAssociative

type Fixity<'e, 'o, 'u> =
    | Prefix of Parser<'o, 'u> * ('e -> 'e)
    | Postfix of Parser<'o, 'u> * ('e -> 'e)
    | Infix of Associativity * Parser<'o, 'u> * ('e -> 'e -> 'e)
    | Ternary of Associativity * Parser<'o, 'u> * Parser<'o, 'u> * ('e -> 'e -> 'e -> 'e)

let inline infixr parser make = Infix (RightAssociative, %parser |>> ignore, make)
let inline infixrt parser make = Infix (RightAssociative, %parser |>> ignore, fun x y -> make (x, y))
let inline infixl parser make = Infix (LeftAssociative, %parser |>> ignore, make)
let inline infixlt parser make = Infix (LeftAssociative, %parser |>> ignore, fun x y -> make (x, y))

let inline prefix parser make = Prefix (%parser |>> ignore, make)
let inline postfix parser make = Postfix (%parser |>> ignore, make)

let inline ternaryl left right make =
    Ternary (LeftAssociative, %left |>> ignore, %right |>> ignore, make)
let inline ternarylt left right make =
    Ternary (LeftAssociative, %left |>> ignore, %right |>> ignore, fun x y z -> make (x, y, z))
let inline ternaryr left right make =
    Ternary (RightAssociative, %left |>> ignore, %right |>> ignore, make)
let inline ternaryrt left right make =
    Ternary (RightAssociative, %left |>> ignore, %right |>> ignore, fun x y z -> make (x, y, z))

type OperatorTable<'e, 'o, 'u> =
    {
        /// Function which, given the overall expression parser,
        /// will produce the parser for a simple term (i.e. literal, ident, or parenthesized subexpr).
        /// Does not need to consume trailing whitespace.
        Term : Parser<'e, 'u> -> Parser<'e, 'u>
        /// Whitespace/comments which may optionally appear between each term in an expression.
        Whitespace : Parser<unit, 'u>
        /// All suppported operators, from highest to lowest precedence (i.e. multiplication before addition).
        /// Operators in the same sub-list have equal precedence.
        Operators : Fixity<'e, 'o, 'u> list list
    }

type private FastPrefixOperator<'e> =
    {
        Make : 'e -> 'e
        Precedence : int
    }

type private TernaryTrailing<'e, 'o, 'u> =
    {
        MinInnerPrecedence : int
        RightOperator : Parser<'o, 'u>
        MinRightPrecedence : int
        Make : 'e -> 'e -> 'e -> 'e
    }

type private FastTrailingOperator<'e, 'o, 'u> =
    | PostfixTrailing of ('e -> 'e)
    | InfixTrailing of int * ('e -> 'e -> 'e)
    | TernaryTrailing of TernaryTrailing<'e, 'o, 'u>

type private FastOperatorTable<'e, 'o, 'u> =
    {
        /// Function which, given the overall expression parser,
        /// will produce the parser for a simple term (i.e. literal, ident, or parenthesized subexpr).
        /// Consumes trailing whitespace, if any.
        Term : Parser<'e, 'u> -> Parser<'e, 'u>
        /// Array indexed by minimum precedence.
        /// Value at `[i]` is a parser that will consume any infix or suffix operator of at least `i` precedence,
        /// consuming trailing whitespace, if any.
        TrailingOperators : Parser<FastTrailingOperator<'e, 'o, 'u>, 'u> array
        /// Parses any prefix operator and trailing whitespace, if any.
        PrefixOperator : Parser<'e FastPrefixOperator, 'u>
    }

let private fastOperatorTable (table : OperatorTable<_, _, _>) =
    let ws = table.Whitespace
    let term expr = table.Term expr .>> ws
    let opsByPrecedence =
        table.Operators |> List.toArray |> Array.rev
    let prefixOps =
        [| for prec, ops in opsByPrecedence |> Seq.indexed do
            for fixity in ops do
                match fixity with
                | Prefix (parser, make) -> yield parser .>> ws >>% { Make = make; Precedence = prec }
                | Postfix _ | Infix _ | Ternary _  -> ()
        |]
    let trailingOps = Array.zeroCreate opsByPrecedence.Length
    for prec = trailingOps.Length - 1 downto 0 do
        let operators =
            [| for fixity in opsByPrecedence.[prec] do
                match fixity with
                | Prefix _ -> ()
                | Postfix (parser, make) -> yield parser .>> ws >>% PostfixTrailing make
                | Infix (assoc, parser, make) ->
                    let nextPrec =
                        match assoc with
                        | LeftAssociative -> prec + 1
                        | RightAssociative -> prec
                    yield parser .>> ws >>% InfixTrailing (nextPrec, make)
                | Ternary (assoc, left, right, make) ->
                    let trailing =
                        {
                            MinInnerPrecedence = prec
                            RightOperator = right .>> ws
                            MinRightPrecedence =
                                match assoc with
                                | LeftAssociative -> prec + 1
                                | RightAssociative -> prec
                            Make = make
                        } |> TernaryTrailing
                    yield left .>> ws >>% trailing
                        
            |]
        let afterHigher =
            if prec + 1 >= trailingOps.Length then id else (<|>) trailingOps.[prec + 1]
        trailingOps.[prec] <- choice operators |> afterHigher
    {
        Term = term
        TrailingOperators = trailingOps
        PrefixOperator = choice prefixOps
    }

let private fastOperatorParser (table : FastOperatorTable<'e, _, 'u>) =
    let ofSelf expr =
        let term = table.Term expr
        let trailingParts : Parser<'e -> 'e, 'u> array =
            Array.zeroCreate table.TrailingOperators.Length
        let exprParts : Parser<'e, 'u> array =
            Array.zeroCreate (table.TrailingOperators.Length + 1)
        let prefix =
            table.PrefixOperator >>= fun op -> exprParts.[op.Precedence] |>> op.Make
        let prefixOrTerm =
            prefix <|> term
        let applySequentially e (xforms : _ ResizeArray) =
            let mutable output = e
            for i = 0 to xforms.Count - 1 do
                output <- xforms.[i] output
            output
        for i, trailing in table.TrailingOperators |> Seq.indexed do
            let nextPart = function
                | PostfixTrailing make ->
                    preturn make
                | InfixTrailing (minRightPrecedence, make) ->
                    exprParts.[minRightPrecedence] |>> fun x y -> make y x
                | TernaryTrailing ternary ->
                    %% +.exprParts.[ternary.MinInnerPrecedence]
                    -- ternary.RightOperator
                    -- +.exprParts.[ternary.MinRightPrecedence]
                    -%> fun inner right left -> ternary.Make left inner right
            trailingParts.[i] <-
                table.TrailingOperators.[i] >>= nextPart
            exprParts.[i] <-
                %% +.prefixOrTerm
                -- +.(trailingParts.[i] * qty.[0..])
                -%> applySequentially
        // for when left-assoc ops are highest precedence, we need one higher level that does no operator parsing
        exprParts.[exprParts.Length - 1] <- prefixOrTerm
        exprParts.[0]
    precursive ofSelf

let expression (table : OperatorTable<_, _, _>) =
    fastOperatorTable table
    |> fastOperatorParser

