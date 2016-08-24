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
    private
    | LeftAssociative
    | RightAssociative

type Fixity<'e> =
    private
    | Infix of Associativity * ('e -> 'e -> 'e)
    | Prefix of ('e -> 'e)
    | Postfix of ('e -> 'e)

let infixr make = Infix (RightAssociative, make)
let infixrt make = Infix (RightAssociative, fun x y -> make (x,y))
let infixl make = Infix (LeftAssociative, make)
let infixlt make = Infix (LeftAssociative, fun x y -> make (x,y))

let prefix make = Prefix make
let postfix make = Postfix make

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
        Operators : (Parser<'o, 'u> * 'e Fixity) list list
    }

type private FastPrefixOperator<'e> =
    {
        Make : 'e -> 'e
        Precedence : int
    }

type private FastTrailingOperator<'e> =
    | InfixTrailing of int * ('e -> 'e -> 'e)
    | PostfixTrailing of ('e -> 'e)

type private FastOperatorTable<'e, 'u> =
    {
        /// Function which, given the overall expression parser,
        /// will produce the parser for a simple term (i.e. literal, ident, or parenthesized subexpr).
        /// Consumes trailing whitespace, if any.
        Term : Parser<'e, 'u> -> Parser<'e, 'u>
        /// Array indexed by minimum precedence.
        /// Value at `[i]` is a parser that will consume any infix or suffix operator of at least `i` precedence,
        /// consuming trailing whitespace, if any.
        TrailingOperators : Parser<'e FastTrailingOperator, 'u> array
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
            for parser, fixity in ops do
                match fixity with
                | Prefix make -> yield parser .>> ws >>% { Make = make; Precedence = prec }
                | Infix _ | Postfix _ -> ()
        |]
    let trailingOps = Array.zeroCreate opsByPrecedence.Length
    for prec = trailingOps.Length - 1 downto 0 do
        let operators =
            [| for parser, fixity in opsByPrecedence.[prec] do
                match fixity with
                | Prefix _ -> ()
                | Infix (assoc, make) ->
                    let nextPrec =
                        match assoc with
                        | LeftAssociative -> prec + 1
                        | RightAssociative -> prec
                    yield parser .>> ws >>% InfixTrailing (nextPrec, make)
                | Postfix make -> yield parser .>> ws >>% PostfixTrailing make
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
                | InfixTrailing (minRightPrecedence, make) -> exprParts.[minRightPrecedence] |>> (fun x y -> make y x)
                | PostfixTrailing make -> preturn make
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

