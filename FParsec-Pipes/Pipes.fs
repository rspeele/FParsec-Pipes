[<AutoOpen>]
module FParsec.Pipes.Pipes
open FParsec

type IPipeLink<'inp, 'out, 'u> =
    abstract member ToFunctionParser : Parser<'inp -> 'out, 'u>
    abstract member ToOutputParser : 'inp -> Parser<'out, 'u>
    abstract member LinkUp : Parser<'up, 'u> -> IPipeLink<'up -> 'inp, 'out, 'u>
    abstract member IgnoreUp : Parser<'up, 'u> -> IPipeLink<'inp, 'out, 'u>

let private supplyInput (link : IPipeLink<_, _, _>) inp = link.ToOutputParser inp
let private linkUp parser (link : IPipeLink<_, _, _>) = link.LinkUp parser
let private ignoreUp parser (link : IPipeLink<_, _, _>) = link.IgnoreUp parser

/// This combines parsers with more than 5 arguments.
/// The trick is to start a new link chain from 1 on the left side, and on the right side keep a parser
/// which produces a function taking a function taking the remaining parsed arguments.
/// The resulting parsers will be equivalent to having written something like (example for 8 arguments):
/// `pipe2 (pipe3 pa pb pc (fun a b c d e f g h -> final_result)) (pipe5 pd pe pf pg ph (fun d e f g h parent -> parent d e f g h)) (|>)`
let rec private linkBeyond5<'a, 'b, 'f, 'u> (functionLink : IPipeLink<'a, 'b, 'u>) (parseRemaining : Parser<'b -> 'f, 'u>) : IPipeLink<'a, 'f, 'u> =
    { new IPipeLink<'a, 'f, 'u> with
        member __.ToFunctionParser = pipe2 (functionLink.ToFunctionParser) parseRemaining (>>)
        member __.ToOutputParser f = pipe2 (functionLink.ToOutputParser f) parseRemaining (|>)
        member __.LinkUp up = linkBeyond5 (functionLink.LinkUp up) parseRemaining
        member __.IgnoreUp up = linkBeyond5 (functionLink.IgnoreUp up) parseRemaining
    }

let rec private link5 a b c d e =
    let p5 = pipe5 a b c d e (fun a b c d e f -> f a b c d e)
    { new IPipeLink<_, _, _> with
        member __.ToFunctionParser = p5
        member __.ToOutputParser f = pipe5 a b c d e f
        member __.LinkUp up =
            linkBeyond5 (link1 up) p5
        member __.IgnoreUp up = link5 (up >>. a) b c d e
    }

and private link4 a b c d =
    { new IPipeLink<_, _, _> with
        member __.ToFunctionParser = pipe4 a b c d (fun a b c d f -> f a b c d)
        member __.ToOutputParser f = pipe4 a b c d f
        member __.LinkUp up = link5 up a b c d
        member __.IgnoreUp up = link4 (up >>. a) b c d
    }

and private link3 a b c =
    { new IPipeLink<_, _, _> with
        member __.ToFunctionParser = pipe3 a b c (fun a b c f -> f a b c)
        member __.ToOutputParser f = pipe3 a b c f
        member __.LinkUp up = link4 up a b c
        member __.IgnoreUp up = link3 (up >>. a) b c
    }

and private link2 a b =
    { new IPipeLink<_, _, _> with
        member __.ToFunctionParser = pipe2 a b (fun a b f -> f a b)
        member __.ToOutputParser f = pipe2 a b f
        member __.LinkUp up = link3 up a b
        member __.IgnoreUp up = link2 (up >>. a) b
    }

// Extra type annotation on link1 is necessary due to its usage in link5.
and private link1<'a, 'b, 'u> (a : Parser<'a, 'u>) : IPipeLink<'a -> 'b, 'b, 'u> =
    { new IPipeLink<'a -> 'b, 'b, 'u> with
        member __.ToFunctionParser = a |>> (|>)
        member __.ToOutputParser f = a |>> f
        member __.LinkUp up = link2 up a
        member __.IgnoreUp up = link1 (up >>. a)
    }

let rec private linkIgnored p =
    { new IPipeLink<'a, 'a, 'u> with
        member __.ToFunctionParser = p >>% id
        member __.ToOutputParser f = p >>% f
        member __.LinkUp up = link1 (up .>> p)
        member __.IgnoreUp up = linkIgnored (up >>. p)
    }

let rec link0 () =
    { new IPipeLink<_, _, _> with
        member __.ToFunctionParser = preturn id
        member __.ToOutputParser f = preturn f
        member __.LinkUp up = link1 up
        member __.IgnoreUp up = linkIgnored up
    }

/// Given the input to the `IPipeLink` we have right now, get a new `IPipeLink` expecting a function
/// that does something with the output of our link. This lets us map functions onto the eventual
/// materialized parser with `|>>`.
let private collapse (link : IPipeLink<'a, 'b, 'u>) (inp : 'a) =
    link.ToOutputParser inp |> link1

type Pipe<'inp, 'out, 'fn, 'r, 'u> =
    | Pipe of (IPipeLink<'inp, 'out, 'u> -> 'fn -> Parser<'r, 'u>)
    
[<GeneralizableValue>]
let pipe<'inp, 'out, 'u> : Pipe<'inp, 'out, 'inp, 'out, 'u> =
    Pipe <| supplyInput

let (|-+) (Pipe parent) (parser : Parser<_, _>) =
    Pipe <|
    fun link fn ->
        parent (linkUp parser link) fn

let (|--) (Pipe parent) (parser : Parser<_, _>) =
    Pipe <|
    fun link fn ->
        parent (ignoreUp parser link) fn

let (|?+) (Pipe parent) (parser : Parser<_, _>) =
    Pipe <|
    fun link fn ->
        let first = parent(link0()) fn
        let next = (linkUp parser link).ToFunctionParser
        pipe2 (attempt first) next (|>)

let (|?-) (Pipe parent) (parser : Parser<_, _>) =
    Pipe <|
    fun link fn ->
        let first = parent(link0()) fn
        let next = (ignoreUp parser link).ToFunctionParser
        pipe2 (attempt first) next (|>)

let (--<|) (Pipe parent) fn =
    Pipe <|
    fun link ->
        parent (collapse link fn)

let (-|>) (Pipe parent) fn : Parser<_, _> =
    parent (link0()) fn

let (--|>) parent fn =
    Pipe <| (linkUp (parent -|> fn) >> supplyInput)