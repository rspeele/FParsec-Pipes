namespace FParsec.Pipes.Test
open FParsec.Pipes.Test.Tools
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open FParsec.Pipes

[<TestClass>]
type TestCapturedInputs() =
    [<TestMethod>]
    member __.TestOne() =
        let parser =
            %% +."test" -%> id
        bad parser "" 0
        bad parser "tes" 0
        bad parser " test" 0
        good parser "test" 4 "test"
        good parser "test stuff" 4 "test"

    [<TestMethod>]
    member __.TestTwo() =
        let parser =
            %% +."one" -- +."two" -%> fun x y -> (x, y)
        bad parser "" 0
        bad parser "onetw" 3
        bad parser " onetwo" 0
        good parser "onetwo" 6 ("one", "two")
        good parser "onetwoasdf" 6 ("one", "two")

    [<TestMethod>]
    member __.TestTwoWithIgnored() =
        let parser =
            %% "one" -- "two" -- +."three" -- "four" -- +."five" -%> fun x y -> (x, y)
        bad parser "" 0
        bad parser "onetw" 3
        bad parser "onetwothree4five" "onetwothree".Length
        bad parser "onetwothreefourfi" "onetwothreefour".Length
        bad parser " onetwothreefourfive" 0
        good parser "onetwothreefourfive" "onetwothreefourfive".Length ("three", "five")
        good parser "onetwothreefourfivek" "onetwothreefourfive".Length ("three", "five")

    [<TestMethod>]
    member __.TestThreeWithIgnored() =
        let parser =
            %% +."one" -- "two" -- +."three" -- "four" -- +."five" -%> fun x y z -> (x, y, z)
        bad parser "" 0
        bad parser "onetw" 3
        bad parser "onetwothre" 6
        bad parser "onetwothree" "onetwothree".Length
        bad parser "onetwothree4five" "onetwothree".Length
        bad parser "onetwothreefourfi" "onetwothreefour".Length
        bad parser " onetwothreefourfive" 0
        good parser "onetwothreefourfive" "onetwothreefourfive".Length ("one", "three", "five")
        good parser "onetwothreefourfivek" "onetwothreefourfive".Length ("one", "three", "five")

    [<TestMethod>]
    member __.TestThreeWithIgnoredAndAttempt() =
        let parser =
            %% +."one" -- "two" -- +."three" ?- "four" -- +."five" -%> fun x y z -> (x, y, z)
        bad parser "" 0
        bad parser "onetw" 0
        bad parser "onetwothre" 0
        bad parser "onetwothree" "onetwothree".Length
        bad parser "onetwothree4five" "onetwothree".Length
        bad parser "onetwothreefourfi" "onetwothreefour".Length
        bad parser " onetwothreefourfive" 0
        good parser "onetwothreefourfive" "onetwothreefourfive".Length ("one", "three", "five")
        good parser "onetwothreefourfivek" "onetwothreefourfive".Length ("one", "three", "five")
