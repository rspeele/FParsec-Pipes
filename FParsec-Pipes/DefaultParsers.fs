[<AutoOpen>]
module FParsec.Pipes.DefaultParsers
open FParsec

/// Represents a parser whose output is ignored within a pipeline.
type Ignore<'a, 'u> =
    | Ignore of Parser<'a, 'u>
    static member (---) (pipe, Ignore (p : Parser<'a, 'u>)) = appendIgnore pipe p
    static member (?--) (pipe, Ignore (p : Parser<'a, 'u>)) = appendIgnoreBacktrackLeft pipe p
    static member (--?) (pipe, Ignore (p : Parser<'a, 'u>)) = appendIgnoreBacktrackRight pipe p

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
            (^a : (static member get_DefaultParser : unit -> Parser< ^a, unit >)()) |> Ignore
and DefaultParser =
    | DefaultParser
    static member inline (%!!~~%) (DefaultParser, cap : Capture<'a, 'u>) = cap

    static member inline (%!!~~%) (DefaultParser, existing : Parser<'a, 'u>) = existing |> Ignore
    static member inline (%!!~~%) (DefaultParser, existing : Parser<'a, 'u> seq) = choice existing |> Ignore

    static member inline (%!!~~%) (DefaultParser, literal : char) = pchar literal |> Ignore
    static member inline (%!!~~%) (DefaultParser, literal : string) = pstring literal |> Ignore
    static member inline (%!!~~%) (DefaultParser, predicate : char -> bool) = satisfy predicate |> Ignore

    static member inline (%!!~~%) (DefaultParser, anyOfThese : char list) = anyOf anyOfThese |> Ignore
    static member inline (%!!~~%) (DefaultParser, anyOfThese : string list) = choice (List.map pstring anyOfThese) |> Ignore

    static member inline (%!!~~%) (DefaultParser, (count, parser)) = parray count parser |> Ignore

    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<char>) = anyChar |> Ignore
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<float>) = pfloat |> Ignore
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<int8>) = pint8 |> Ignore
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<int16>) = pint16 |> Ignore
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<int32>) = pint32 |> Ignore
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<int64>) = pint64 |> Ignore
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<uint8>) = puint8 |> Ignore
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<uint16>) = puint16 |> Ignore
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<uint32>) = puint32 |> Ignore
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<uint64>) = puint64 |> Ignore
    static member inline (%!!~~%) (DefaultParser, _ : DefaultParserOf<Position>) = getPosition |> Ignore

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
    let (Ignore parser) = DefaultParser %!!~~% x
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