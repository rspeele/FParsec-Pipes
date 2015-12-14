#r @"bin/debug/FParsecCS.dll"
#r @"bin/debug/FParsec.dll"
#r @"bin/debug/FParsec-Pipes.dll"
#nowarn "193"
open System
open FParsec
open FParsec.Pipes

(**

# FParsec-Pipes: making FParsec parsers more concise and readable.

This library defines new operators for FParsec.

Some of these operators make defining common parsers, like those based
on `pstring`, `pchar`, and `choice`, nearly automatic.

Others make the task of composing parsers together more consistent
and easier to think through.

## What does a parser look like written with these operators?

*)

let digits (count : int) =
    !+ (count, digit) -|> (String >> Int32.Parse)

let date =
    !+ digits(4) -- '-' -+ digits(2) -- '-' -+ digits(2)
    -|> fun yyyy mm dd -> (yyyy, mm, dd)

let time =
    !+ digits(2) -- ':' -+ digits(2) -- ':' -+ digits(2)
    -|> fun hh mm ss -> (hh, mm, ss)

let iso8601 : Parser<_, unit> =
    !+ date -- 'T' -+ time
    -|> fun (yyyy, mm, dd) (hh, mi, ss) ->
        new DateTime(yyyy, mm, dd, hh, mi, ss)
(**

## How do I use it?

There are two main things to learn.

### Default parsers for values

First, you can use the prefix operator `%` on many types of values to
create the parser that sensibly corresponds to that value. For example:
*)
// Expression:              // Equivalent to:
%"bob"                      // pstring "bob"
%'b'                        // pchar 'b'
%['a'; 'b']                 // anyOf "ab"
%["alice"; "bob"]           // choice [pstring "alice"; pstring "bob"]
%[pint32; preturn (-1)]     // choice [pint32; preturn (-1)]
%(3, digit)                 // parray 3 digit

(**

The value you pass to `%` can be of type `DefaultParserOf<'a>`, which you
can obtain by using `auto<'a>`. If `'a` is a supported type, the resulting
parser will be one to parse any `'a`.

This works for several primitive types, including all integer types.

*)

// Expression:    // Equivalent to:
%auto<int>        // pint32
%auto<uint16>     // puint16
%auto<float>      // pfloat
%auto<Position>   // getPosition

(**

You can make `auto` work for your own types, too. Simply define:

`static member DefaultParser : Parser<'a, 'u>`

*)

type Vector3D =
    {
        X : float
        Y : float
        Z : float
    }
    static member DefaultParser =
        let comma = spaces >>. %',' >>. spaces
        pipe3
            (%'(' >>. spaces >>. %auto<float>)
            (comma >>. %auto<float>)
            (comma >>. %auto<float> .>> spaces .>> %')')
            (fun x y z -> { X = x; Y = y; Z = z })

%auto<Vector3D> // equivalent to Vector3D.DefaultParser

(**
### Pipes

Notice that the above example used the function `pipe3`, and the combinators `>>.` and `.>>`.

The main feature of FParsec-Pipes is a set of operators that replace those for most use cases.
*)

//
//        !- '(' -- spaces
//        -+ auto<float>
//        -- spaces -- ',' -- spaces
//        -+ auto<float>
//        -- spaces -- ',' -- spaces
//        -+ auto<float>
//        -- spaces -- ')'
//        -|> fun x y z -> { X = x; Y = y; Z = z }