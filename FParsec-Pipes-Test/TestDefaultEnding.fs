namespace FParsec.Pipes.Test
open FParsec.Pipes.Test.Tools
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open FParsec.Pipes

[<TestClass>]
type TestDefaultEnding() =
    [<TestMethod>]
    member __.TestOne() =
        let parser = %% "pre" -- +.p<int64> -%> auto
        bad parser "" 0
        bad parser "pre" 3
        good parser "pre123" 6 123L
    [<TestMethod>]
    member __.TestTwo() =
        let parser = %% "pre" -- +.p<int64> -- ',' -- +.p<float> -%> auto
        bad parser "" 0
        bad parser "pre" 3
        bad parser "pre123" 6
        good parser "pre123,5.1" 10 (123L, 5.1)
    [<TestMethod>]
    member __.TestThree() =
        let parser = %% "pre" -- +.p<int64> -- ',' -- +.p<float> -- +."term" -%> auto
        bad parser "" 0
        bad parser "pre" 3
        bad parser "pre123" 6
        bad parser "pre123,5.1" 10
        bad parser "pre123,5.1ter" 10
        good parser "pre123,5.1term" 14 (123L, 5.1, "term")