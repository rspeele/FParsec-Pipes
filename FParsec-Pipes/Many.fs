[<AutoOpen>]
module FParsec.Pipes.Many
open FParsec

let parseManyRange min max (parser : Parser<'a, 'u>) (stream : CharStream<'u>) =
    let output = new ResizeArray<'a>(capacity = min)
    let mutable count = 0
    let mutable looping = count < max
    let mutable lastStatus = ReplyStatus.Error
    let mutable lastError = FParsec.Error.NoErrorMessages
    let mutable exitWithError = false
    while looping do
        let tag = stream.StateTag
        let reply = parser stream
        lastStatus <- reply.Status
        lastError <- reply.Error
        if lastStatus = Ok then
            output.Add(reply.Result)
            count <- count + 1
            looping <- count < max
        else
            exitWithError <- tag <> stream.StateTag
            looping <- false
    if not exitWithError && count >= min then Reply(output) 
    else Reply(lastStatus, output, lastError)

let parseManyRangeSepBy allowEnd min max (sep : Parser<'b, 'u>) (parser : Parser<'a, 'u>) (stream : CharStream<'u>) =
    let output = new ResizeArray<'a>(capacity = min)
    let mutable count = 0
    let mutable looping = count < max
    let mutable lastStatus = ReplyStatus.Error
    let mutable lastError = FParsec.Error.NoErrorMessages
    let mutable exitWithError = false
    while looping do
        let mutable sepSucceeded = false
        if count <> 0 then
            let tag = stream.StateTag
            let sepReply = sep stream
            sepSucceeded <- sepReply.Status = Ok
            lastStatus <- sepReply.Status
            lastError <- sepReply.Error
            looping <- sepSucceeded
            exitWithError <- not sepSucceeded && tag <> stream.StateTag
        if looping then
            let tag = stream.StateTag
            let reply = parser stream
            lastStatus <- reply.Status
            lastError <- reply.Error
            if lastStatus = Ok then
                output.Add(reply.Result)
                count <- count + 1
                looping <- count < max
            else
                exitWithError <- (not allowEnd && sepSucceeded) || tag <> stream.StateTag
                looping <- false
    if not exitWithError && count >= min then Reply(output) 
    else Reply(lastStatus, output, lastError)

let parseManyUnbounded min (parser : Parser<'a, 'u>) (stream : CharStream<'u>) =
    let output = new ResizeArray<'a>(capacity = min)
    let mutable count = 0
    let mutable looping = true
    let mutable lastStatus = ReplyStatus.Error
    let mutable lastError = FParsec.Error.NoErrorMessages
    let mutable exitWithError = false
    while looping do
        let tag = stream.StateTag
        let reply = parser stream
        lastStatus <- reply.Status
        lastError <- reply.Error
        if reply.Status = Ok then
            if stream.StateTag = tag then
                failwith
                    ( "Infinite loop detected: "
                    + "parsing an unbounded range of a parser "
                    + "that succeeds without consuming input" )
            output.Add(reply.Result)
            count <- count + 1
        else
            exitWithError <- tag <> stream.StateTag
            looping <- false
    if exitWithError || count < min then Reply(lastStatus, output, lastError)
    else Reply(output)

let parseManyUnboundedSepBy allowEnd min (sep : Parser<'b, 'u>) (parser : Parser<'a, 'u>) (stream : CharStream<'u>) =
    let output = new ResizeArray<'a>(capacity = min)
    let mutable count = 0
    let mutable looping = true
    let mutable lastStatus = ReplyStatus.Error
    let mutable lastError = FParsec.Error.NoErrorMessages
    let mutable exitWithError = false
    while looping do
        let mutable sepSucceeded = false
        if count <> 0 then
            let tag = stream.StateTag
            let sepReply = sep stream
            sepSucceeded <- sepReply.Status = Ok
            lastStatus <- sepReply.Status
            lastError <- sepReply.Error
            looping <- sepSucceeded
            exitWithError <- not sepSucceeded && tag <> stream.StateTag
        if looping then 
            let tag = stream.StateTag
            let reply = parser stream
            lastStatus <- reply.Status
            lastError <- reply.Error
            if reply.Status = Ok then
                if stream.StateTag = tag then
                    failwith
                        ( "Infinite loop detected: "
                        + "parsing an unbounded range of a parser "
                        + "that succeeds without consuming input" )
                output.Add(reply.Result)
                count <- count + 1
            else
                exitWithError <- (not allowEnd && sepSucceeded) || tag <> stream.StateTag
                looping <- false
    if exitWithError || count < min then Reply(lastStatus, output, lastError)
    else Reply(output)

type ZeroOrOne =
    | ZeroOrOne
    static member inline ( * ) (ZeroOrOne, x) = opt %x
    static member inline ( * ) (x, ZeroOrOne) = opt %x

type Range(min : int, max : int option) =
    member this.Min = min
    member this.Max = max
    member this.Of (p : Parser<'a, 'u>) =
        match max with
        | Some 0 -> preturn (new ResizeArray<'a>())
        | None -> parseManyUnbounded min p
        | Some max -> parseManyRange min max p

    static member inline ( * ) (range : Range, x) = range.Of(%x)
    static member inline ( * ) (x, range : Range) = range.Of(%x)
    static member inline ( / ) (range : Range, x) = RangeSeparatedBy(range, %x, false)
    static member inline ( /. ) (range : Range, x) = RangeSeparatedBy(range, %x, true)

and RangeSeparatedBy<'a, 'u>(range : Range, separator : Parser<'a, 'u>, allowEnd : bool) =
    member this.Of(parser: Parser<'b, 'u>) =
        match range.Max with
        | None -> parseManyUnboundedSepBy allowEnd range.Min separator parser
        | Some max -> parseManyRangeSepBy allowEnd range.Min max separator parser

    static member inline ( * ) (rangeSep : RangeSeparatedBy<_, _>, x) = rangeSep.Of(%x)
    static member inline ( * ) (x, rangeSep : RangeSeparatedBy<_, _>) = rangeSep.Of(%x)

and ManyVariant =
    | ZeroOrMore
    | OneOrMore
    member this.Of(parser : Parser<'a, 'u>) =
        match this with
        | ZeroOrMore -> many parser
        | OneOrMore -> many1 parser
    member this.SeparatedBy(parser : Parser<'a, 'u>, allowEnd : bool) =
        ManySeparatedBy<'a, 'u>(parser, this, allowEnd)
    static member inline ( * ) (many : ManyVariant, x) = many.Of(%x)
    static member inline ( * ) (x, many : ManyVariant) = many.Of(%x)
    static member inline ( / ) (many : ManyVariant, x) = many.SeparatedBy(%x, allowEnd = false)
    static member inline ( /. ) (many : ManyVariant, x) = many.SeparatedBy(%x, allowEnd = true)

and ManySeparatedBy<'a, 'u>(separator : Parser<'a, 'u>, variant : ManyVariant, allowEnd : bool) =
    member this.Of(parser : Parser<'b, 'u>) =
        match variant, allowEnd with
        | ZeroOrMore, false -> sepBy parser separator
        | OneOrMore, false -> sepBy1 parser separator
        | ZeroOrMore, true -> sepEndBy parser separator
        | OneOrMore, true -> sepEndBy1 parser separator

    static member inline ( * ) (sep : ManySeparatedBy<_, _>, x) = sep.Of(%x)
    static member inline ( * ) (x, sep : ManySeparatedBy<_, _>) = sep.Of(%x)

let zeroOrOne = ZeroOrOne
let zeroOrMore = ZeroOrMore
let oneOrMore = OneOrMore
let (<=..<=) min max = Range(min, Some max)
let atLeast min = Range(min, None)
let exactly count = Range(count, Some count)

let ``0..1`` = ZeroOrOne
let ``0..*`` = ZeroOrMore
let ``1..*`` = OneOrMore
