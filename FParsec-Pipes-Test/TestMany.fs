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
        let parser = element * qty.[1..] |>> List.ofSeq
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
        let element = !+ "ab" -+ "c " -|> (+)
        let parser = element * qty.[3..5] |>> List.ofSeq
        good parser "abc abc abc " 12 ["abc "; "abc "; "abc "]
        bad parser "abc abc " 8
        good parser "abc abc abc abc test" 16 ["abc "; "abc "; "abc "; "abc "]
        bad parser "abc abc abc abc abXX" 18
        good parser "abc abc abc abc abc " 20 ["abc "; "abc "; "abc "; "abc "; "abc "]
        good parser "abc abc abc abc abc abc abc abc " 20 ["abc "; "abc "; "abc "; "abc "; "abc "]

    [<TestMethod>]
    member __.TestUnboundedRangeSep() =
        let element = !+ "a" -+ "b" -|> (+)
        let parser = element * (qty.[1..] / ',') |>> List.ofSeq
        let comparison = sepBy1 element %','
        bad parser "" 0
        bad comparison "" 0
        bad parser "a" 1
        bad comparison "a" 1
        good parser "ab" 2 ["ab"]
        good comparison  "ab" 2 ["ab"]
        good parser "ab,ab" 5 ["ab"; "ab"]
        good comparison "ab,ab" 5 ["ab"; "ab"]
        bad parser "ab,a" 4
        bad comparison "ab,a" 4
        good parser "ab,ab,ab,ab" 11 ["ab"; "ab"; "ab"; "ab"]
        good comparison "ab,ab,ab,ab" 11 ["ab"; "ab"; "ab"; "ab"]
        bad parser "ab,ab,ab,ab," 12
        bad comparison "ab,ab,ab,ab," 12

    [<TestMethod>]
    member __.TestUnboundedRangeSepEnd() =
        let element = !+ "a" -+ "b" -|> (+)
        let parser = element * (qty.[1..] /. ',') |>> List.ofSeq
        let comparison = sepEndBy1 element %','
        bad parser "" 0
        bad comparison "" 0
        bad parser "a" 1
        bad comparison "a" 1
        good parser "ab" 2 ["ab"]
        good comparison  "ab" 2 ["ab"]
        good parser "ab,ab" 5 ["ab"; "ab"]
        good comparison "ab,ab" 5 ["ab"; "ab"]
        bad parser "ab,a" 4
        bad comparison "ab,a" 4
        good parser "ab,ab,ab,ab" 11 ["ab"; "ab"; "ab"; "ab"]
        good comparison "ab,ab,ab,ab" 11 ["ab"; "ab"; "ab"; "ab"]
        good parser "ab,ab,ab,ab," 12 ["ab"; "ab"; "ab"; "ab"]
        good comparison "ab,ab,ab,ab," 12 ["ab"; "ab"; "ab"; "ab"]

    [<TestMethod>]
    member __.TestBoundedRangeSep() =
        let element = !+ "a" -+ "b" -|> (+)
        let parser = element * (qty.[3..4] / ',') |>> List.ofSeq
        let comparison = sepBy1 element %','
        bad parser "" 0
        bad parser "a" 1
        bad parser "ab" 2
        bad parser "ab,ab" 5
        bad parser "ab,a" 4
        good parser "ab,ab,ab" 8 ["ab"; "ab"; "ab"]
        bad parser "ab,ab,ab," 9
        good parser "ab,ab,ab,ab" 11 ["ab"; "ab"; "ab"; "ab"]
        good parser "ab,ab,ab,ab," 11

    [<TestMethod>]
    member __.TestBoundedRangeSepEnd() =
        let element = !+ "a" -+ "b" -|> (+)
        let parser = element * (qty.[1..] /. ',') |>> List.ofSeq
        let comparison = sepBy1 element %','
        bad parser "" 0
        bad parser "a" 1
        good parser "ab" 2 ["ab"]
        good parser "ab,ab" 5 ["ab"; "ab"]
        bad parser "ab,a" 4
        good parser "ab,ab,ab,ab" 11 ["ab"; "ab"; "ab"; "ab"]
        good parser "ab,ab,ab,ab," 12 ["ab"; "ab"; "ab"; "ab"]
        
        