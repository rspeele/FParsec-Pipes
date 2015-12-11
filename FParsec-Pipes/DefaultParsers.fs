[<AutoOpen>]
module FParsec.Pipes.DefaultParsers
open FParsec

[<AllowNullLiteral>]
type DefaultParserOf<'a>() =
    static member Instance = null : DefaultParserOf<'a>
and [<AllowNullLiteral>]
    CustomDefaultParserOf< ^a when ^a : (static member get_DefaultParser : unit -> Parser< ^a, unit >) >() =
        static member inline (%%%%) (DefaultParser, _ : CustomDefaultParserOf< ^a >) =
            (^a : (static member get_DefaultParser : unit -> Parser< ^a, unit >)())
and DefaultParser =
    | DefaultParser
    static member inline (%%%%) (DefaultParser, existing : Parser<'a, 'u>) = existing
    static member inline (%%%%) (DefaultParser, existing : Parser<'a, 'u> seq) = choice existing

    static member inline (%%%%) (DefaultParser, literal : char) = pchar literal
    static member inline (%%%%) (DefaultParser, literal : string) = pstring literal
    static member inline (%%%%) (DefaultParser, numLitOpts : NumberLiteralOptions) = numberLiteral numLitOpts
    static member inline (%%%%) (DefaultParser, predicate : char -> bool) = satisfy predicate
    static member inline (%%%%) (DefaultParser, anyOfThese : char list) = anyOf anyOfThese

    static member inline (%%%%) (DefaultParser, (a, b)) = tuple2 a b
    static member inline (%%%%) (DefaultParser, (a, b, c)) = tuple3 a b c
    static member inline (%%%%) (DefaultParser, (a, b, c, d)) = tuple4 a b c d
    static member inline (%%%%) (DefaultParser, (a, b, c, d, e)) = tuple5 a b c d e

    static member inline (%%%%) (DefaultParser, _ : DefaultParserOf<char>) = anyChar
    static member inline (%%%%) (DefaultParser, _ : DefaultParserOf<float>) = pfloat
    static member inline (%%%%) (DefaultParser, _ : DefaultParserOf<int8>) = pint8
    static member inline (%%%%) (DefaultParser, _ : DefaultParserOf<int16>) = pint16
    static member inline (%%%%) (DefaultParser, _ : DefaultParserOf<int32>) = pint32
    static member inline (%%%%) (DefaultParser, _ : DefaultParserOf<int64>) = pint64
    static member inline (%%%%) (DefaultParser, _ : DefaultParserOf<uint8>) = puint8
    static member inline (%%%%) (DefaultParser, _ : DefaultParserOf<uint16>) = puint16
    static member inline (%%%%) (DefaultParser, _ : DefaultParserOf<uint32>) = puint32
    static member inline (%%%%) (DefaultParser, _ : DefaultParserOf<uint64>) = puint64
    static member inline (%%%%) (DefaultParser, _ : DefaultParserOf<Position>) = getPosition

    static member inline (%%%%) (DefaultParser, _ : DefaultParserOf< ^a >) = DefaultParser %%%% (null : CustomDefaultParserOf< ^a >)

let parse<'a> = DefaultParserOf<'a>.Instance

type DefaultEnding =
    | DefaultEnding
    static member inline (|%>) (pipe, DefaultEnding) : Parser<unit, _> = pipe |=> ()
    static member inline (|%>) (pipe, DefaultEnding) : Parser<'a, _> = pipe |=> (id : 'a -> 'a)
    static member inline (|%>) (pipe, DefaultEnding) : Parser<_ * _, _> = pipe |=> (fun a b -> (a, b))
    static member inline (|%>) (pipe, DefaultEnding) : Parser<_ * _ * _, _> = pipe |=> (fun a b c -> (a, b, c))
    static member inline (|%>) (pipe, DefaultEnding) : Parser<_ * _ * _ * _, _> = pipe |=> (fun a b c d -> (a, b, c, d))
    static member inline (|%>) (pipe, DefaultEnding) : Parser<_ * _ * _ * _ * _, _> = pipe |=> (fun a b c d e -> (a, b, c, d, e))

/// Create a parser from x, if there is a single sensible parser possible.
let inline (~%) x = DefaultParser %%%% x

let inline (|--) pipe x = pipe |--- %x
let inline (|-+) pipe x = pipe |--+ %x
let inline (|?-) pipe x = pipe |-?- %x
let inline (|?+) pipe x = pipe |-?+ %x

let auto = DefaultEnding
