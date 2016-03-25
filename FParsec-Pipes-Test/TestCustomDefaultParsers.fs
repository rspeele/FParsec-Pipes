namespace FParsec.Pipes.Test
open FParsec.Pipes.Test.Tools
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open FParsec.Pipes

type CustomA =
    | CustomA of float
    static member DefaultParser =
        %% "a:" -- spaces -+ auto<float> -|> CustomA

type CustomB =
    | CustomB of char
    static member DefaultParser =
        %% "b:" -- spaces -+ auto<char> -|> CustomB

[<TestClass>]
type TestCustomDefaultParsers() =
    [<TestMethod>]
    member __.TestCustomA() =
        let parser = %auto<CustomA>
        bad parser "" 0
        bad parser "a" 0
        bad parser "a: " 3
        bad parser "b: 1.5" 0
        good parser "a: 1.5" 6 (CustomA 1.5)
        good parser "a: 0" 4 (CustomA 0.0)

    [<TestMethod>]
    member __.TestCustomB() =
        let parser = %auto<CustomB>
        bad parser "" 0
        bad parser "a" 0
        bad parser "b" 0
        bad parser "b: " 3
        good parser "b: 1.5" 4 (CustomB '1')
        good parser "b: 0" 4 (CustomB '0')
        good parser "b: nc" 4 (CustomB 'n')