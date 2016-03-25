[<AutoOpen>]
module FParsec.Pipes.DefaultParsers
open FParsec

type Ignore<'a, 'u> =
    | Ignore of Parser<'a, 'u>
    static member (---) (pipe, Ignore (p : Parser<'a, 'u>)) = pipe |-- p
    static member (?--) (pipe, Ignore (p : Parser<'a, 'u>)) = pipe |?- p

type Capture<'a, 'u> =
    | Capture of Parser<'a, 'u>
    static member (---) (pipe, Capture (p : Parser<'a, 'u>)) = pipe |-+ p
    static member (?--) (pipe, Capture (p : Parser<'a, 'u>)) = pipe |?+ p

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

let auto<'a> = DefaultParserOf<'a>.Instance

type DefaultEnding =
    | DefaultEnding
    static member inline (-%>) (pipe, DefaultEnding) : Parser<unit, _> =
        pipe -%> ()
    static member inline (-%>) (pipe, DefaultEnding) : Parser<'a, _> =
        pipe -%> (id : 'a -> 'a)
    static member inline (-%>) (pipe, DefaultEnding) : Parser<_ * _, _> =
        pipe -%> (fun a b -> (a, b))
    static member inline (-%>) (pipe, DefaultEnding) : Parser<_ * _ * _, _> =
        pipe -%> (fun a b c -> (a, b, c))
    static member inline (-%>) (pipe, DefaultEnding) : Parser<_ * _ * _ * _, _> =
        pipe -%> (fun a b c d -> (a, b, c, d))
    static member inline (-%>) (pipe, DefaultEnding) : Parser<_ * _ * _ * _ * _, _> =
        pipe -%> (fun a b c d e -> (a, b, c, d, e))

let autofun = DefaultEnding

/// Create a parser from x, if there is a single sensible parser possible.
let inline defaultParser x =
    let (Ignore parser) = DefaultParser %!!~~% x
    parser

let inline (~+.) x = Capture (defaultParser x)

let inline (--) pipe x = pipe --- (DefaultParser %!!~~% x)
let inline (?-) pipe x = pipe ?-- (DefaultParser %!!~~% x)

let inline (~%) x = defaultParser x
let inline (~%%) x = pipe -- x

let precursive defineParser =
    let p, pref = createParserForwardedToRef()
    pref := defineParser p
    p