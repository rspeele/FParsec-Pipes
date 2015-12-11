[<AutoOpen>]
module FParsec.Pipes.Test.Tools
open FParsec

let private checkConsume (stream : #CharStream) consumed =
    if int stream.Position.Index <> consumed then
        failwith
            ("Expected to consume "
                + string consumed
                + "; actually consumed "
                + string stream.Position.Index)

let good (parser : Parser<'a, 'u>) (input : string) consumed (expected : 'a) : unit =
    let stream = new CharStream<'u>(input, 0, input.Length)
    let reply = parser stream
    match reply.Status with
    | Ok ->
        if reply.Result <> expected then
            failwith
                ("Expected to parse "
                    + expected.ToString()
                    + "; got "
                    + reply.Result.ToString())
        checkConsume stream consumed
    | _ ->
        failwith <|
        (new ParserError(stream.Position, stream.State, reply.Error)).ToString()

let bad (parser : Parser<'a, 'u>) (input : string) consumed : unit =
    let stream = new CharStream<'u>(input, 0, input.Length)
    let reply = parser stream
    match reply.Status with
    | Ok ->
        failwith
            ("Parser succeeded with "
                + reply.Result.ToString()
                + "; expected failure")
    | _ ->
        checkConsume stream consumed