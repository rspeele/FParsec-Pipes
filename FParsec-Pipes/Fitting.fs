namespace FParsec.Pipes
open FParsec

[<AbstractClass>]
type Appendable() = // marker base class
    class end

[<AbstractClass>]
type IgnoreAppendable<'u>() =
    inherit Appendable()
    abstract member Append : Pipe<'inp, 'out, 'fn, 'r, 'u> -> Pipe<'inp, 'out, 'fn, 'r, 'u>
    abstract member AppendBacktrackLeft : Pipe<'inp, 'inp, 'fn, 'r, 'u> -> Pipe<'r, 'x, 'fn, 'x, 'u>
    abstract member AppendBacktrackRight : Pipe<'inp, 'inp, 'fn, 'r, 'u> -> Pipe<'r, 'x, 'fn, 'x, 'u>

[<AbstractClass>]
type CaptureAppendable<'a, 'u>() =
    inherit Appendable()
    abstract member Append : Pipe<'a -> 'inp, 'out, 'fn, 'r, 'u> -> Pipe<'inp, 'out, 'fn, 'r, 'u>
    abstract member AppendBacktrackLeft : Pipe<'inp, 'inp, 'fn, 'a -> 'r, 'u> -> Pipe<'r, 'x, 'fn, 'x, 'u>
    abstract member AppendBacktrackRight : Pipe<'inp, 'inp, 'fn, 'a -> 'r, 'u> -> Pipe<'r, 'x, 'fn, 'x, 'u>

[<AbstractClass>]
type Fitting<'a, 'u, 'app when 'app :> Appendable>(parser : Parser<'a, 'u>) =
    member __.Parser = parser
    abstract member Appendable : 'app

[<Sealed>]
type IgnoreFitting<'a, 'u>(parser : Parser<'a, 'u>) =
    inherit Fitting<'a, 'u, IgnoreAppendable<'u>>(parser)
    override __.Appendable =
        { new IgnoreAppendable<'u>() with
            override __.Append(pipe) = appendIgnore pipe parser
            override __.AppendBacktrackLeft(pipe) = appendIgnoreBacktrackLeft pipe parser
            override __.AppendBacktrackRight(pipe) = appendIgnoreBacktrackRight pipe parser
        }

[<Sealed>]
type CaptureFitting<'a, 'u>(parser : Parser<'a, 'u>) =
    inherit Fitting<'a, 'u, CaptureAppendable<'a, 'u>>(parser)
    override __.Appendable =
        { new CaptureAppendable<'a, 'u>() with
            override __.Append(pipe) = appendCapture pipe parser
            override __.AppendBacktrackLeft(pipe) = appendCaptureBacktrackLeft pipe parser
            override __.AppendBacktrackRight(pipe) = appendCaptureBacktrackRight pipe parser
        }
    

