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

let inline (---) (pipe : Pipe<'inp, 'out, 'fn, 'r, 'u>) (fitting : Fitting<'a, 'u, ^app>) =
    (^app : (member Append : Pipe<'inp, 'out, 'fn, 'r, 'u> -> Pipe<_, 'out, 'fn, 'r, 'u>)
        (fitting.Appendable, pipe))

let inline (?--) (pipe : Pipe<'inp, 'inp, 'fn, 'r, 'u>) (fitting : Fitting<'a, 'u, ^app>) =
    (^app : (member AppendBacktrackLeft : Pipe<'inp, 'inp, 'fn, 'r, 'u> -> Pipe<'r, 'x, 'fn, 'x, 'u>)
        (fitting.Appendable, pipe))

let inline (--?) (pipe : Pipe<'inp, 'inp, 'fn, 'r, 'u>) (fitting : Fitting<'a, 'u, ^app>) =
    (^app : (member AppendBacktrackRight : Pipe<'inp, 'inp, 'fn, 'r, 'u> -> Pipe<_, 'x, 'fn, 'x, 'u>)
        (fitting.Appendable, pipe))

type DefaultFitting =
    | DefaultFitting
    static member inline (%!!&&%) (DefaultFitting, parser : Parser<_, _>) =
        IgnoreFitting(parser) :> Fitting<_, _, _>
    static member inline (%!!&&%) (DefaultFitting, fitting : Fitting<_, _, _>) =
        fitting

[<AllowNullLiteral>]
type DefaultParserOf<'a>() =
    class end
and [<AllowNullLiteral>]
    CustomDefaultParserOf< ^a when ^a : (static member get_DefaultParser : unit -> Parser< ^a, unit >) >() =
        static member inline (%!!~~%) (DefaultParser, _ : CustomDefaultParserOf< ^a >) =
            (^a : (static member get_DefaultParser : unit -> Parser< ^a, unit >)())
and DefaultParser =
    | DefaultParser
    static member inline (%!!~~%) (DefaultParser, cap : Fitting<_, _, _>) = cap

    static member inline (%!!~~%) (DefaultParser, existing : Parser<'a, 'u>) = existing
    static member inline (%!!~~%) (DefaultParser, existing : Parser<'a, 'u> seq) = choice existing

    static member inline (%!!~~%) (DefaultParser, literal : char) = pchar literal
    static member inline (%!!~~%) (DefaultParser, literal : string) = pstring literal
    static member inline (%!!~~%) (DefaultParser, CaseInsensitive (literal : char)) = pcharCI literal 
    static member inline (%!!~~%) (DefaultParser, CaseInsensitive (literal : string)) = pstringCI literal 
    static member inline (%!!~~%) (DefaultParser, predicate : char -> bool) = satisfy predicate 

    static member inline (%!!~~%) (DefaultParser, anyOfThese : char list) =
        anyOf anyOfThese 
    static member inline (%!!~~%) (DefaultParser, anyOfThese : string list) =
        choice (List.map pstring anyOfThese) 
    static member inline (%!!~~%) (DefaultParser, anyOfThese : CaseInsensitive<char> list) =
        choice (anyOfThese |> List.map (function CaseInsensitive s -> pcharCI s)) 
    static member inline (%!!~~%) (DefaultParser, anyOfThese : CaseInsensitive<string> list) =
        choice (anyOfThese |> List.map (function CaseInsensitive s -> pstringCI s)) 

    static member inline (%!!~~%) (DefaultParser, (count, parser)) = parray count parser 

    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<char>) = anyChar 
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<float>) = pfloat 
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<int8>) = pint8 
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<int16>) = pint16 
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<int32>) = pint32 
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<int64>) = pint64 
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<uint8>) = puint8 
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<uint16>) = puint16 
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<uint32>) = puint32 
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<uint64>) = puint64 
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<Position>) = getPosition 

    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf< ^a >) =
        DefaultParser %!!~~% (null : CustomDefaultParserOf< ^a >)

/// Represents the default parser for the given type.
/// If the type `'a` has a default parser implemented, `p<'a>`
/// can be converted to a `Parser<'a, 'u>` with the % operator,
/// e.g. `%p<int>`.
let p<'a> = null : DefaultParserOf<'a>

/// Create a parser from `x`, if there is a single sensible parser possible.
/// For example, `defaultParser "str"` is equivalent to `pstring str`.
let inline defaultParser x =
    DefaultParser %!!~~% x : Parser<_, _>

let inline defaultFitting x =
    DefaultFitting %!!&&% (DefaultParser %!!~~% x) : Fitting<_, _, _>

/// Converts its argument to a parser via `defaultParser` and
/// marks the result as a captured input, which can be consumed
/// by the function at the end of a pipe.
let inline (~+.) x = CaptureFitting(defaultParser x)

/// Chains `parser` onto `pipe`.
/// `parser` will be converted to a parser and may be captured or ignored based
/// on whether `+.` was used on it.
let inline (--) pipe parser =
    pipe --- defaultFitting parser

/// Chains `parser` onto `pipe`, with backtracking if `pipe` fails prior to `parser`.
/// `parser` will be converted to a parser and may be captured or ignored based
/// on whether `+.` was used on it.
let inline (?-) pipe parser =
    pipe ?-- defaultFitting parser

/// Chains `parser` onto `pipe`, with backtracking if `pipe` fails prior to `parser`
/// or `parser` fails without changing the parser state.
/// `parser` will be converted to a parser and may be captured or ignored based
/// on whether `+.` was used on it.
let inline (-?) pipe parser =
    pipe --? defaultFitting parser

/// Creates a pipe starting with `parser`. Shorthand for `pipe -- parser`.
let inline (~%%) parser : Pipe<_, _, _, _, _> =
    pipe -- parser

/// Prefix operator equivalent to `defaultParser x`.
let inline (~%) x = defaultParser x

/// Defines a self-referential parser given `defineParser`, which returns a parser given its own output parser.
/// The parser that will be passed to `defineParser` is a `createParserForwardedToRef()` pointed at a reference
/// that will be assigned to `defineParser`'s output.
let precursive defineParser =
    let p, pref = createParserForwardedToRef()
    pref := defineParser p
    p