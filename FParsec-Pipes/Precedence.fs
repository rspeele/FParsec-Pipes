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

type Fixity<'e, 'u> =
    | Prefix of Parser<'e -> 'e, 'u>
    | Postfix of Parser<'e -> 'e, 'u>
    | Infix of Associativity * Parser<'e -> 'e -> 'e, 'u>
    | Ternary of Associativity * Parser<'e -> 'e -> 'e -> 'e, 'u> * Parser<unit, 'u>

let inline infixr parser make = Infix (RightAssociative, %parser >>% make)
let inline infixrt parser make = Infix (RightAssociative, %parser >>% fun x y -> make (x, y))
let inline infixl parser make = Infix (LeftAssociative, %parser >>% make)
let inline infixlt parser make = Infix (LeftAssociative, %parser >>% fun x y -> make (x, y))

let inline prefix parser make = Prefix (%parser >>% make)
let inline postfix parser make = Postfix (%parser >>% make)

let inline ternaryl left right make =
    Ternary (LeftAssociative, %left >>% make, %right |>> ignore)
let inline ternarylt left right make =
    Ternary (LeftAssociative, (%left >>% fun x y z -> make (x, y, z)), %right |>> ignore)
let inline ternaryr left right make =
    Ternary (RightAssociative, %left >>% make, %right |>> ignore)
let inline ternaryrt left right make =
    Ternary (RightAssociative, (%left >>% fun x y z -> make (x, y, z)), %right |>> ignore)

type OperatorTable<'e, 'u> =
    {
        /// Function which, given the overall expression parser,
        /// will produce the parser for a simple term (i.e. literal, ident, or parenthesized subexpr).
        /// Does not need to consume trailing whitespace.
        Term : Parser<'e, 'u> -> Parser<'e, 'u>
        /// Whitespace/comments which may optionally appear between each term in an expression.
        Whitespace : Parser<unit, 'u>
        /// All suppported operators, from highest to lowest precedence (i.e. multiplication before addition).
        /// Operators in the same sub-list have equal precedence.
        Operators : Fixity<'e, 'u> list list
    }

type private FastPrefixOperator<'e> =
    {
        Make : 'e -> 'e
        Precedence : int
    }

type private TernaryTrailing<'e, 'u> =
    {
        MinInnerPrecedence : int
        RightOperator : Parser<unit, 'u>
        MinRightPrecedence : int
        Make : 'e -> 'e -> 'e -> 'e
    }

type private FastTrailingOperator<'e, 'u> =
    | PostfixTrailing of ('e -> 'e)
    | InfixTrailing of int * ('e -> 'e -> 'e)
    | TernaryTrailing of TernaryTrailing<'e, 'u>

type private FastOperatorTable<'e, 'u> =
    {
        /// Function which, given the overall expression parser,
        /// will produce the parser for a simple term (i.e. literal, ident, or parenthesized subexpr).
        /// Consumes trailing whitespace, if any.
        Term : Parser<'e, 'u> -> Parser<'e, 'u>
        /// Array indexed by minimum precedence.
        /// Value at `[i]` is a parser that will consume any infix or suffix operator of at least `i` precedence,
        /// consuming trailing whitespace, if any.
        TrailingOperators : Parser<FastTrailingOperator<'e, 'u>, 'u> array
        /// Parses any prefix operator and trailing whitespace, if any.
        PrefixOperator : Parser<'e FastPrefixOperator, 'u>
    }

let private fastOperatorTable (table : OperatorTable<_, _>) =
    let ws = table.Whitespace
    let term expr = table.Term expr .>> ws
    let opsByPrecedence =
        table.Operators |> List.toArray |> Array.rev
    let prefixOps =
        [| for prec, ops in opsByPrecedence |> Seq.indexed do
            for fixity in ops do
                match fixity with
                | Prefix parser ->
                    yield %% +.parser -- ws -%> fun make -> { Make = make; Precedence = prec }
                | Postfix _ | Infix _ | Ternary _  -> ()
        |]
    let trailingOps = Array.zeroCreate opsByPrecedence.Length
    for prec = trailingOps.Length - 1 downto 0 do
        let operators =
            [| for fixity in opsByPrecedence.[prec] do
                match fixity with
                | Prefix _ -> ()
                | Postfix parser ->
                    yield %% +.parser -- ws -%> PostfixTrailing
                | Infix (assoc, parser) ->
                    let nextPrec =
                        match assoc with
                        | LeftAssociative -> prec + 1
                        | RightAssociative -> prec
                    yield %% +.parser -- ws -%> fun make -> InfixTrailing (nextPrec, make)
                | Ternary (assoc, left, right) ->
                    let trailing make =
                        {
                            MinInnerPrecedence = prec
                            RightOperator = right .>> ws
                            MinRightPrecedence =
                                match assoc with
                                | LeftAssociative -> prec + 1
                                | RightAssociative -> prec
                            Make = make
                        } |> TernaryTrailing
                    yield %% +.left -- ws -%> trailing
                        
            |]
        let afterHigher =
            if prec + 1 >= trailingOps.Length then id else (<|>) trailingOps.[prec + 1]
        trailingOps.[prec] <- choice operators |> afterHigher
    {
        Term = term
        TrailingOperators = trailingOps
        PrefixOperator = choice prefixOps
    }

let private fastOperatorParser (table : FastOperatorTable<'e, 'u>) =
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

let expression (table : OperatorTable<_, _>) =
    fastOperatorTable table
    |> fastOperatorParser

