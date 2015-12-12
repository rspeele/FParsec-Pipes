namespace FParsec.Pipes.Test
open FParsec.Pipes.Test.Tools
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open FParsec.Pipes

[<TestClass>]
type TestDefaultParsers() =
    [<TestMethod>]
    member __.TestInt64() =
        let parser = %auto<int64>
        bad parser "" 0
        bad parser "a" 0
        good parser "123" 3 123L

    [<TestMethod>]
    member __.TestUInt64() =
        let parser = %auto<uint64>
        bad parser "" 0
        bad parser "a" 0
        good parser "123" 3 123UL

    [<TestMethod>]
    member __.TestInt32() =
        let parser = %auto<int32>
        bad parser "" 0
        bad parser "a" 0
        good parser "123" 3 123

    [<TestMethod>]
    member __.TestUInt32() =
        let parser = %auto<uint32>
        bad parser "" 0
        bad parser "a" 0
        good parser "123" 3 123u

    [<TestMethod>]
    member __.TestInt16() =
        let parser = %auto<int16>
        bad parser "" 0
        bad parser "a" 0
        good parser "123" 3 123s

    [<TestMethod>]
    member __.TestUInt16() =
        let parser = %auto<uint16>
        bad parser "" 0
        bad parser "a" 0
        good parser "123" 3 123us

    [<TestMethod>]
    member __.TestFloat() =
        let parser = %auto<float>
        bad parser "" 0
        good parser "1" 1 1.0
        good parser "1.0" 3 1.0
        good parser "1.5" 3 1.5
        good parser "1.5e-3" 6 1.5e-3

    [<TestMethod>]
    member __.TestChar() =
        let parser = %auto<char>
        bad parser "" 0
        good parser "a" 1 'a'
        good parser "b" 1 'b'
        good parser "ab" 1 'a'

    [<TestMethod>]
    member __.TestChoice() =
        let parser = %[pstring "abc" >>% 1; pstring "def" >>% 2]
        bad parser "" 0
        good parser "abc" 3 1
        good parser "def" 3 2
        good parser "abcdef" 3 1

    [<TestMethod>]
    member __.TestStringChoice() =
        let parser = %["abc"; "def"]
        bad parser "" 0
        good parser "abc" 3 "abc"
        good parser "def" 3 "def"
        good parser "abcdef" 3 "abc"

    [<TestMethod>]
    member __.TestCharChoice() =
        let parser = %['a'; 'b']
        bad parser "" 0
        good parser "a" 1 'a'
        good parser "b" 1 'b'
        good parser "ab" 1 'a'

    [<TestMethod>]
    member __.TestCharPredicate() =
        let parser = %(fun c -> c = 'x' || c = 'y')
        bad parser "" 0
        bad parser "z" 0
        good parser "x" 1 'x'
        good parser "y" 1 'y'