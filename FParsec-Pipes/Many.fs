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
module FParsec.Pipes.Many
open FParsec

let private parseManyRange min max (parser : Parser<'a, 'u>) (stream : CharStream<'u>) =
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

let private parseManyRangeSepBy allowEnd min max (sep : Parser<'b, 'u>) (parser : Parser<'a, 'u>) (stream : CharStream<'u>) =
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

let private parseManyUnbounded min (parser : Parser<'a, 'u>) (stream : CharStream<'u>) =
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

let private parseManyUnboundedSepBy allowEnd min (sep : Parser<'b, 'u>) (parser : Parser<'a, 'u>) (stream : CharStream<'u>) =
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

type ExactRange(count) =
    member this.Count = count
    static member inline ( * ) (range : ExactRange, x) = parray range.Count %x
    static member inline ( * ) (x, range : ExactRange) = parray range.Count %x
    static member inline ( / ) (range : ExactRange, x) = ExactRangeSeparatedBy(range.Count, %x, false)
    static member inline ( /. ) (range : ExactRange, x) = ExactRangeSeparatedBy(range.Count, %x, true)

and ExactRangeSeparatedBy<'a, 'u>(count : int, separator : Parser<'a, 'u>, allowEnd : bool) =
    member this.Of(parser: Parser<'b, 'u>) =
        parseManyRangeSepBy allowEnd count count separator parser
        |>> (fun ra -> ra.ToArray())

    static member inline ( * ) (rangeSep : ExactRangeSeparatedBy<_, _>, x) = rangeSep.Of(%x)
    static member inline ( * ) (x, rangeSep : ExactRangeSeparatedBy<_, _>) = rangeSep.Of(%x)

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

let zeroOrOne = ZeroOrOne

type RangeDefiner() =
    member this.GetSlice(min : int option, max : int option) =
        new Range(defaultArg min 0, max)
    member this.Item(exactCount : int) =
        new ExactRange(exactCount)

let qty = RangeDefiner()

