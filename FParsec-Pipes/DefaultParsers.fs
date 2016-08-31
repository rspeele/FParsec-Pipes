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
module FParsec.Pipes.DefaultParsers
open FParsec

type CaseInsensitive<'a> =
    | CaseInsensitive of 'a

/// Mark `x` as being case insensitive.
/// Useful for use with `%`. For example `%ci "test"` is equivalent
/// to `pstringCI "test"`, while `%"test"` is equivalent to `pstring "test"`.
let ci x = CaseInsensitive x

/// Parse a character case insensitively. Returns the parsed character.
let pcharCI c : Parser<char, 'u> =
    let cfs = Text.FoldCase(c : char)
    fun stream ->
        let index0 = stream.IndexToken
        if stream.SkipCaseFolded(cfs) then
             Reply(stream.Peek(-1))
        else Reply(Error, expectedString (string c))

/// Represents a parser whose output is ignored within a pipeline.
type Ignore<'a, 'u> =
    | IgnoreParser of Parser<'a, 'u>
    static member (---) (pipe, IgnoreParser (p : Parser<'a, 'u>)) = appendIgnore pipe p
    static member (?--) (pipe, IgnoreParser (p : Parser<'a, 'u>)) = appendIgnoreBacktrackLeft pipe p
    static member (--?) (pipe, IgnoreParser (p : Parser<'a, 'u>)) = appendIgnoreBacktrackRight pipe p

/// Reprsents a parser whose output is captured within a pipeline.
type Capture<'a, 'u> =
    | Capture of Parser<'a, 'u>
    static member (---) (pipe, Capture (p : Parser<'a, 'u>)) = appendCapture pipe p
    static member (?--) (pipe, Capture (p : Parser<'a, 'u>)) = appendCaptureBacktrackLeft pipe p
    static member (--?) (pipe, Capture (p : Parser<'a, 'u>)) = appendCaptureBacktrackRight pipe p

[<AllowNullLiteral>]
type DefaultParserOf<'a>() =
    static member Instance = null : DefaultParserOf<'a>
and [<AllowNullLiteral>]
    CustomDefaultParserOf< ^a when ^a : (static member get_DefaultParser : unit -> Parser< ^a, unit >) >() =
        static member inline (%!!~~%) (DefaultParser, _ : CustomDefaultParserOf< ^a >) =
            (^a : (static member get_DefaultParser : unit -> Parser< ^a, unit >)()) |> IgnoreParser
and DefaultParser =
    | DefaultParser
    static member inline (%!!~~%) (DefaultParser, cap : Capture<'a, 'u>) = cap

    static member inline (%!!~~%) (DefaultParser, existing : Parser<'a, 'u>) = existing |> IgnoreParser
    static member inline (%!!~~%) (DefaultParser, existing : Parser<'a, 'u> seq) = choice existing |> IgnoreParser

    static member inline (%!!~~%) (DefaultParser, literal : char) = pchar literal |> IgnoreParser
    static member inline (%!!~~%) (DefaultParser, literal : string) = pstring literal |> IgnoreParser
    static member inline (%!!~~%) (DefaultParser, CaseInsensitive (literal : char)) = pcharCI literal |> IgnoreParser
    static member inline (%!!~~%) (DefaultParser, CaseInsensitive (literal : string)) = pstringCI literal |> IgnoreParser
    static member inline (%!!~~%) (DefaultParser, predicate : char -> bool) = satisfy predicate |> IgnoreParser

    static member inline (%!!~~%) (DefaultParser, anyOfThese : char list) =
        anyOf anyOfThese |> IgnoreParser
    static member inline (%!!~~%) (DefaultParser, anyOfThese : string list) =
        choice (List.map pstring anyOfThese) |> IgnoreParser
    static member inline (%!!~~%) (DefaultParser, anyOfThese : CaseInsensitive<char> list) =
        choice (anyOfThese |> List.map (function CaseInsensitive s -> pcharCI s)) |> IgnoreParser
    static member inline (%!!~~%) (DefaultParser, anyOfThese : CaseInsensitive<string> list) =
        choice (anyOfThese |> List.map (function CaseInsensitive s -> pstringCI s)) |> IgnoreParser

    static member inline (%!!~~%) (DefaultParser, (count, parser)) = parray count parser |> IgnoreParser

    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<char>) = anyChar |> IgnoreParser
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<float>) = pfloat |> IgnoreParser
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<int8>) = pint8 |> IgnoreParser
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<int16>) = pint16 |> IgnoreParser
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<int32>) = pint32 |> IgnoreParser
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<int64>) = pint64 |> IgnoreParser
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<uint8>) = puint8 |> IgnoreParser
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<uint16>) = puint16 |> IgnoreParser
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<uint32>) = puint32 |> IgnoreParser
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<uint64>) = puint64 |> IgnoreParser
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<Position>) = getPosition |> IgnoreParser

    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf< ^a >) =
        DefaultParser %!!~~% (null : CustomDefaultParserOf< ^a >)

/// Represents the default parser for the given type.
/// If the type `'a` has a default parser implemented, `p<'a>`
/// can be converted to a `Parser<'a, 'u>` with the % operator,
/// e.g. `%p<int>`.
let p<'a> = DefaultParserOf<'a>.Instance

/// Create a parser from `x`, if there is a single sensible parser possible.
/// For example, `defaultParser "str"` is equivalent to `pstring str`.
let inline defaultParser x =
    let (IgnoreParser parser) = DefaultParser %!!~~% x
    parser

/// Converts its argument to a parser via `defaultParser` and
/// marks the result as a captured input, which can be consumed
/// by the function at the end of a pipe.
let inline (~+.) x = Capture (defaultParser x)

/// Chains `parser` onto `pipe`.
/// `parser` will be converted to a parser and may be captured or ignored based
/// on whether `+.` was used on it.
let inline (--) pipe parser = pipe --- (DefaultParser %!!~~% parser)

/// Chains `parser` onto `pipe`, with backtracking if `pipe` fails prior to `parser`.
/// `parser` will be converted to a parser and may be captured or ignored based
/// on whether `+.` was used on it.
let inline (?-) pipe parser = pipe ?-- (DefaultParser %!!~~% parser)

/// Chains `parser` onto `pipe`, with backtracking if `pipe` fails prior to `parser`
/// or `parser` fails without changing the parser state.
/// `parser` will be converted to a parser and may be captured or ignored based
/// on whether `+.` was used on it.
let inline (-?) pipe parser = pipe --? (DefaultParser %!!~~% parser)

/// Creates a pipe starting with `parser`. Shorthand for `pipe -- parser`.
let inline (~%%) parser = pipe -- parser

/// Prefix operator equivalent to `defaultParser x`.
let inline (~%) x = defaultParser x

/// Defines a self-referential parser given `defineParser`, which returns a parser given its own output parser.
/// The parser that will be passed to `defineParser` is a `createParserForwardedToRef()` pointed at a reference
/// that will be assigned to `defineParser`'s output.
let precursive defineParser =
    let p, pref = createParserForwardedToRef()
    pref := defineParser p
    p