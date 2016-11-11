namespace FParsec.Pipes
open FParsec

[<AbstractClass>]
type Appendable() = // marker base class
    class end

type IgnoreAppendable<'a, 'u>() =
    inherit Appendable()
    static let instance = IgnoreAppendable<'a, 'u>()
    member __.Append(parser, pipe) = appendIgnore pipe parser
    member __.AppendBacktrackLeft(parser, pipe) = appendIgnoreBacktrackLeft pipe parser
    member __.AppendBacktrackRight(parser, pipe) = appendIgnoreBacktrackRight pipe parser
    static member Instance = instance

type CaptureAppendable<'a, 'u>() =
    inherit Appendable()
    static let instance = CaptureAppendable<'a, 'u>()
    member __.Append(parser, pipe) = appendCapture pipe parser
    member __.AppendBacktrackLeft(parser, pipe) = appendCaptureBacktrackLeft pipe parser
    member __.AppendBacktrackRight(parser, pipe) = appendCaptureBacktrackRight pipe parser
    static member Instance = instance

[<AbstractClass>]
type Fitting<'a, 'u, 'app when 'app :> Appendable>(parser : Parser<'a, 'u>) =
    member __.Parser = parser
    abstract member Appendable : 'app

[<Sealed>]
type IgnoreFitting<'a, 'u>(parser : Parser<'a, 'u>) =
    inherit Fitting<'a, 'u, IgnoreAppendable<'a, 'u>>(parser)
    override __.Appendable = IgnoreAppendable<'a, 'u>.Instance

[<Sealed>]
type CaptureFitting<'a, 'u>(parser : Parser<'a, 'u>) =
    inherit Fitting<'a, 'u, CaptureAppendable<'a, 'u>>(parser)
    override __.Appendable = CaptureAppendable<'a, 'u>.Instance
    

