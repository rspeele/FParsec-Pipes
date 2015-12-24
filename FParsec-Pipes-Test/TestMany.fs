namespace FParsec.Pipes.Test
open FParsec.Pipes.Test.Tools
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open FParsec.Pipes

[<TestClass>]
type TestMany() =
    [<TestMethod>]
    member __.TestUnboundedRange() =
        let element = !+ "test" -+ "stuff" -|> (+)
        let parser = element * atLeast 1 |>> List.ofSeq
        let comparison = many1 element
        bad parser "te" 0
        bad parser "tests" 4
        good parser "teststuffasdf" 9 ["teststuff"]
        good comparison "teststuffasdf" 9 ["teststuff"]
        good parser "teststuffteststuff" "teststuffteststuff".Length ["teststuff"; "teststuff"]
        bad comparison "teststufftestabcd" 13
        bad parser "teststufftestabcd" 13

    [<TestMethod>]
    member __.TestBoundedRange() =
        let element = !+ "test" -+ "stuff" -|> (+)
        let parser = element * (3 <=..<= 5) |>> List.ofSeq
        let comparison = many1 element