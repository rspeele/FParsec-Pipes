namespace FParsec.Pipes.Test
open FParsec.Pipes.Test.Tools
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open FParsec.Pipes

[<TestClass>]
type TestIgnoredInputs() =
    [<TestMethod>]
    member __.TestReturn() =
        let parser =
            pipe
            -|> "ret"
        good parser "" 0 "ret"
        good parser "test" 0 "ret"

    [<TestMethod>]
    member __.TestSingleIgnoredInput() =
        let parser =
            pipe
            -- "required"
            -|> "tsii"
        bad parser "" 0
        bad parser "test" 0 
        good parser "required" "required".Length "tsii"
        good parser "required stuff" "required".Length "tsii"

    [<TestMethod>]
    member __.TestTwoIgnoredInputs() =
        let parser =
            pipe
            -- "req"
            -- "uired"
            -|> "ttii"
        bad parser "" 0
        bad parser "re" 0
        bad parser "req" 3
        bad parser "requi" 3
        good parser "required" "required".Length "ttii"
        good parser "required stuff" "required".Length "ttii"

    [<TestMethod>]
    member __.TestThreeIgnoredInputs() =
        let parser =
            pipe
            -- "ab"
            -- "cd"
            -- "ef"
            -|> "3ii"
        bad parser "" 0
        bad parser "ab" 2
        bad parser "abc" 2
        bad parser "abcd" 4
        bad parser "abcde" 4
        good parser "abcdef" 6 "3ii"
        good parser "abcdef other stuff" 6 "3ii"

    [<TestMethod>]
    member __.TestThreeIgnoredInputsWithAttempt() =
        let parser =
            pipe
            -- "ab"
            -- "cd"
            ?- "ef"
            -|> "3iia"
        bad parser "" 0
        bad parser "ab" 0
        bad parser "abc" 0
        bad parser "abcd" 4
        bad parser "abcde" 4
        good parser "abcdef" 6 "3iia"
        good parser "abcdef other stuff" 6 "3iia"
    
    [<TestMethod>]
    member __.TestFourIgnoredInputsWithAttempt() =
        let parser =
            pipe
            -- "ab"
            -- "cd"
            ?- "ef"
            -- "gh"
            -|> "4iia"
        bad parser "" 0
        bad parser "ab" 0
        bad parser "abc" 0
        bad parser "abcd" 4
        bad parser "abcdef" 6
        bad parser "abcdefg" 6
        good parser "abcdefgh" 8 "4iia"
        good parser "abcdefghmore" 8 "4iia"
        

