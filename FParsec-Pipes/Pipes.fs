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
module FParsec.Pipes.Pipes
open FParsec
open System

/// Represents the right side of a pipeline, which can become a `Parser<'out,'u>`
/// if given a value of type `'inp`. Practically speaking, `'inp` is typically
/// a function combining the outputs of the captured parsers constituting this pipe section.
[<AbstractClass>]
type PipeLink<'inp, 'out, 'u>() =
    /// Convert to a `Parser<'inp -> 'out, 'u>`. This defers the
    /// requirement of an `'inp` value to after parsing, rather than
    /// before creating the parser.
    abstract member ToFunctionParser : Parser<'inp -> 'out, 'u>
    /// Create a `Parser<'out, 'u>` from an `'inp` value.
    abstract member ToOutputParser : 'inp -> Parser<'out, 'u>
    /// Given a parser to a precede this, produce an `IPipeLink` whose `'inp` type is
    /// a function going from the preceding parser's output to our `'inp`.
    /// This lets us build an `IPipeLink` requiring an `a -> b -> c -> d`
    /// from a chain of `parseA`, `parseB`, `parseC`, working right-to-left.
    abstract member LinkUp : Parser<'up, 'u> -> PipeLink<'up -> 'inp, 'out, 'u>
    /// Produce an `IPipeLink` that will run the given parser prior to this one's
    /// and ignore its output.
    abstract member IgnoreUp : Parser<'up, 'u> -> PipeLink<'inp, 'out, 'u>

let private supplyInput (link : PipeLink<_, _, _>) inp = link.ToOutputParser inp
let private linkUp parser (link : PipeLink<_, _, _>) = link.LinkUp parser
let private ignoreUp parser (link : PipeLink<_, _, _>) = link.IgnoreUp parser

// This combines parsers with more than 5 arguments.
// The trick is to start a new link chain from 1 on the left side, and on the right side keep a parser
// which produces a function taking a function taking the remaining parsed arguments.
// The resulting parsers will be equivalent to having written something like (example for 8 arguments):
//     pipe2
//         (pipe3 pa pb pc (fun a b c d e f g h -> final_result))
//         (pipe5 pd pe pf pg ph (fun d e f g h parent -> parent d e f g h)) (|>)
let rec private linkBeyond5<'a, 'b, 'f, 'u>
    (functionLink : PipeLink<'a, 'b, 'u>)
    (parseRemaining : Parser<'b -> 'f, 'u>)
    : PipeLink<'a, 'f, 'u> =
    { new PipeLink<'a, 'f, 'u>() with
        member __.ToFunctionParser = pipe2 (functionLink.ToFunctionParser) parseRemaining (>>)
        member __.ToOutputParser f = pipe2 (functionLink.ToOutputParser f) parseRemaining (|>)
        member __.LinkUp up = linkBeyond5 (functionLink.LinkUp up) parseRemaining
        member __.IgnoreUp up = linkBeyond5 (functionLink.IgnoreUp up) parseRemaining
    }

let rec private link5 a b c d e =
    let p5 = pipe5 a b c d e (fun a b c d e f -> f a b c d e)
    { new PipeLink<_, _, _>() with
        member __.ToFunctionParser = p5
        member __.ToOutputParser f = pipe5 a b c d e f
        member __.LinkUp up =
            linkBeyond5 (link1 up) p5
        member __.IgnoreUp up = link5 (up >>. a) b c d e
    }

and private link4 a b c d =
    { new PipeLink<_, _, _>() with
        member __.ToFunctionParser = pipe4 a b c d (fun a b c d f -> f a b c d)
        member __.ToOutputParser f = pipe4 a b c d f
        member __.LinkUp up = link5 up a b c d
        member __.IgnoreUp up = link4 (up >>. a) b c d
    }

and private link3 a b c =
    { new PipeLink<_, _, _>() with
        member __.ToFunctionParser = pipe3 a b c (fun a b c f -> f a b c)
        member __.ToOutputParser f = pipe3 a b c f
        member __.LinkUp up = link4 up a b c
        member __.IgnoreUp up = link3 (up >>. a) b c
    }

and private link2 a b =
    { new PipeLink<_, _, _>() with
        member __.ToFunctionParser = pipe2 a b (fun a b f -> f a b)
        member __.ToOutputParser f = pipe2 a b f
        member __.LinkUp up = link3 up a b
        member __.IgnoreUp up = link2 (up >>. a) b
    }

// Extra type annotation on link1 is necessary due to its usage in link5.
and private link1<'a, 'b, 'u> (a : Parser<'a, 'u>) : PipeLink<'a -> 'b, 'b, 'u> =
    { new PipeLink<'a -> 'b, 'b, 'u>() with
        member __.ToFunctionParser = a |>> (|>)
        member __.ToOutputParser f = a |>> f
        member __.LinkUp up = link2 up a
        member __.IgnoreUp up = link1 (up >>. a)
    }

let rec private linkIgnored p =
    { new PipeLink<'a, 'a, 'u>() with
        member __.ToFunctionParser = p >>% id
        member __.ToOutputParser f = p >>% f
        member __.LinkUp up = link1 (up .>> p)
        member __.IgnoreUp up = linkIgnored (up >>. p)
    }

let link0 () =
    { new PipeLink<_, _, _>() with
        member __.ToFunctionParser = preturn id
        member __.ToOutputParser f = preturn f
        member __.LinkUp up = link1 up
        member __.IgnoreUp up = linkIgnored up
    }

/// Given the input to the `IPipeLink` we have right now, get a new `IPipeLink` expecting a function
/// that does something with the output of our link. This lets us map functions onto the eventual
/// materialized parser with `|>>`.
let private collapse (link : PipeLink<'a, 'b, 'u>) (inp : 'a) =
    link.ToOutputParser inp |> link1

/// Represents a chain of parsers that can be converted to a single parser,
/// if given a value of type `'fn`. `'fn` is typically a function
/// that takes the captured inputs accumulated by the parser chain
/// and combines them into a single output.
[<NoComparison>]
[<NoEquality>]
type Pipe<'inp, 'out, 'fn, 'r, 'u> =
    | Pipe of (PipeLink<'inp, 'out, 'u> -> 'fn -> Parser<'r, 'u>)
    /// Provide the pipe with the function it requires to become a parser.
    static member (-%>) (Pipe parent, fn : 'fn) : Parser<'r, 'u> =
        parent (link0()) fn
    [<Obsolete("Exists to convince compiler to do overload resolution")>]
    static member (---) (p : Pipe<_, _, _, _, _>, ()) = p
    static member (---) (Pipe parent, parser : Parser<_, _>) =
        Pipe <| fun link fn ->
            parent (ignoreUp parser link) fn
    [<Obsolete("Exists to convince compiler to do overload resolution")>]
    static member (?--) (p : Pipe<_, _, _, _, _>, _ : int) = p
    [<Obsolete("Exists to convince compiler to do overload resolution")>]
    static member (?--) (p : Pipe<_, _, _, _, _>, ()) = p
    static member (?--) (Pipe parent, parser: Parser<_, _>) =
        Pipe <| fun link fn ->
            let first = parent(link0()) fn
            let next = (ignoreUp parser link).ToFunctionParser
            pipe2 (attempt first) next (|>)
    [<Obsolete("Exists to convince compiler to do overload resolution")>]
    static member (--?) (p : Pipe<_, _, _, _, _>, _ : int) = p
    [<Obsolete("Exists to convince compiler to do overload resolution")>]
    static member (--?) (p : Pipe<_, _, _, _, _>, ()) = p
    static member (--?) (Pipe parent, parser : Parser<_, _>) =
        Pipe <| fun link fn ->
            let first = parent(link0()) fn
            let next = (ignoreUp parser link).ToFunctionParser
            attempt first .>>.? next |>> (fun (f, n) -> f |> n)

let private supplyPipeFunction (Pipe parent) fn =
    parent (link0()) fn : Parser<_, _>

/// A marker type which implements the default function to terminate a pipe
/// of a given arity (up to 5).
type DefaultEnding =
    | DefaultEnding
    static member (-%>) (pipe, DefaultEnding) : Parser<'a, _> =
        supplyPipeFunction pipe (id : 'a -> 'a)
    static member (-%>) (pipe, DefaultEnding)
        : Parser<_ * _, _> =
        supplyPipeFunction pipe (fun a b -> (a, b))
    static member (-%>) (pipe, DefaultEnding)
        : Parser<_ * _ * _, _> =
        supplyPipeFunction pipe  (fun a b c -> (a, b, c))
    static member (-%>) (pipe, DefaultEnding)
        : Parser<_ * _ * _ * _, _> =
        supplyPipeFunction pipe  (fun a b c d -> (a, b, c, d))
    static member (-%>) (pipe, DefaultEnding)
        : Parser<_ * _ * _ * _ * _, _> =
        supplyPipeFunction pipe  (fun a b c d e -> (a, b, c, d, e))

/// A marker value which serves as the default function to terminate a pipe.
/// When found on the right side of the `-%>` operator, this behaves equivalently
/// to the most appropriate function for the pipe's arity - producing unit, a single
/// value, or an n-tuple (up to n=5).
let auto = DefaultEnding

/// The starting (leftmost) value of any `Pipe`.
/// This contains no parsers, so if terminated immediately, it will `preturn`
/// the value supplied as a terminator.
[<GeneralizableValue>]
let pipe<'inp, 'out, 'u> : Pipe<'inp, 'out, 'inp, 'out, 'u> =
    Pipe supplyInput

/// Append a parser to a pipe, capturing its output.
let appendCapture (Pipe parent) (parser : Parser<_, _>) =
    Pipe <|
    fun link fn ->
        parent (linkUp parser link) fn

/// Append a parser to a pipe, ignoring its output.
let appendIgnore (Pipe parent) (parser : Parser<_, _>) =
    Pipe <|
    fun link fn ->
        parent (ignoreUp parser link) fn

/// Append a parser to a pipe, capturing its output, with backtracking
/// up to the point that the added parser runs.
/// If the parsers before this one in the pipe fail, even after changing
/// the parser state, the entire pipe will be backtracked.
let appendCaptureBacktrackLeft (Pipe parent) (parser : Parser<_, _>) =
    Pipe <|
    fun link fn ->
        let first = parent(link0()) fn
        let next = (linkUp parser link).ToFunctionParser
        pipe2 (attempt first) next (|>)

/// Append a parser to a pipe, ignoring its output, with backtracking
/// up to the point that the added parser runs.
/// If the parsers before this one in the pipe fail, even after changing
/// the parser state, the entire pipe will be backtracked.
let appendIgnoreBacktrackLeft (Pipe parent) (parser : Parser<_, _>) =
    Pipe <|
    fun link fn ->
        let first = parent(link0()) fn
        let next = (ignoreUp parser link).ToFunctionParser
        pipe2 (attempt first) next (|>)

/// Append a parser to a pipe, capturing its output, with backtracking
/// up to the point that the added parser has consumed input.
/// If the parsers before this one in the pipe fail, even after changing the parser state,
/// or if the added parser fails before changing the parser state, the entire pipe will
/// be backtracked.
let appendCaptureBacktrackRight (Pipe parent) (parser : Parser<_, _>) =
    Pipe <|
    fun link fn ->
        let first = parent(link0()) fn
        let next = (linkUp parser link).ToFunctionParser
        attempt first .>>.? next |>> (fun (f, n) -> f |> n)

/// Append a parser to a pipe, ignoring its output, with backtracking
/// up to the point that the added parser has consumed input.
/// If the parsers before this one in the pipe fail, even after changing the parser state,
/// or if the added parser fails before changing the parser state, the entire pipe will
/// be backtracked.
let appendIgnoreBacktrackRight (Pipe parent) (parser : Parser<_, _>) =
    Pipe <|
    fun link fn ->
        let first = parent(link0()) fn
        let next = (ignoreUp parser link).ToFunctionParser
        attempt first .>>.? next |>> (fun (f, n) -> f |> n)

/// Supply a function partway through a pipe that combines the
/// captured inputs following it.
/// This operator is virtually never useful and is included only for symmetry.
let (--<|) (Pipe parent) fn =
    Pipe <|
    fun link ->
        parent (collapse link fn)

/// Supply a function partway through a pipe that combines the
/// captured inputs preceding it.
/// This operator is rarely useful.
let (--|>) pipe fn =
    Pipe <| (linkUp (supplyPipeFunction pipe fn) >> supplyInput)

/// Provide the pipe with the function it requires to become a parser.
/// In most cases this operator is equivalent to `-%>`.
/// However, `-|>` is not overloaded so `pipe -|> auto` does not work.
let (-|>) pipe fn =
    supplyPipeFunction pipe fn