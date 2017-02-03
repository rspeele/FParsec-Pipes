(*** hide ***)

#r @"bin/debug/FParsecCS.dll"
#r @"bin/debug/FParsec.dll"
#r @"bin/debug/FParsec-Pipes.dll"
#nowarn "193"
open System
open FParsec
open FParsec.Pipes

(**

# FParsec-Pipes: making FParsec parsers even more concise and consistent.

[FParsec](http://www.quanttec.com/fparsec/) is an F# library for writing parsers.
This library extends FParsec with a new set of combinators, which
I believe make it even easier to translate from a formal grammar like
[EBNF](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form)
to F# parsing code and end up with a fast, highly readable parser.

## Show me an example

Here's a parser for ISO-8601-formatted datetimes.

*)

// Parses `count` digits and returns the result as an integer.
let digits (count : int) =
    %% +.(qty.[count] * digit)
    -|> (String >> Int32.Parse)

// Parses a yyyy-mm-dd date and returns the result as a tuple of 3 integers.
let date =
    %% +.digits 4 -- '-' -- +.digits 2 -- '-' -- +.digits 2
    -%> auto

// Parses a hh:mm:ss time and returns the result as a tuple of 3 integers.
let time =
    %% +.digits 2 -- ':' -- +.digits 2 -- ':' -- +.digits 2
    -%> auto

// Parses a DateTime in the format yyyy-mm-ddThh:mm:ss
let datetime : Parser<_, unit> =
    %% +.date -- 'T' -- +.time
    -|> fun (yyyy, mm, dd) (hh, mi, ss) ->
        new DateTime(yyyy, mm, dd, hh, mi, ss)

(**

## What are the features?

### Default parsers for values

The unary operator `%` can be used to convert various values to the "obvious" corresponding parser.
This cuts down on clutter from explicitly using `pstring`, `pchar`, and others.
See the examples below:

*)
// Expression:              // Equivalent to:
%"bob"                      // pstring "bob"
%ci "jim"                   // pstringCI "jim" // case insensitive
%'b'                        // pchar 'b'
%ci 'b'                     // pcharCI 'b' // case insensitive
%['a'; 'b']                 // choice [pchar 'a'; pchar 'b']
%["alice"; "bob"]           // choice [pstring "alice"; pstring "bob"]
%[pint32; preturn (-1)]     // choice [pint32; preturn (-1)]

(**

The value you pass to `%` can also be of type `DefaultParserOf<'a>`, which you
can obtain by using `p<'a>`. If `'a` is a supported type, the resulting
parser will be one to parse any `'a`.

This works for several primitive types, including all integer types.

*)

// Expression:  // Equivalent to:
%p<int>         // pint32
%p<uint16>      // puint16
%p<float>       // pfloat
%p<Position>    // getPosition

(**

You can make `p` work for your own types, too. Simply define:

`static member DefaultParser : Parser<'a, 'u>`

*)

type Vector3D =
    {
        X : float
        Y : float
        Z : float
    }
    // Parses a vector in the format (x, y, z)
    static member DefaultParser =
        let comma = spaces >>. %',' >>. spaces
        pipe3
            (%'(' >>. spaces >>. %p<float>)
            (comma >>. %p<float>)
            (comma >>. %p<float> .>> spaces .>> %')')
            (fun x y z -> { X = x; Y = y; Z = z })

%p<Vector3D> // equivalent to Vector3D.DefaultParser

(**
### Pipes

Notice that the above `DefaultParser` example used the function `pipe3`, and the combinators `>>.` and `.>>`.
Using vanilla FParsec, when you want to make a parser like "`parserA` followed by `parserB` followed by `parserC`",
you must choose between `>>.`, `.>>`, `.>>.`, `pipe2` through `pipe5`, or `tuple2` through `tuple5`, depending on which
of the chained parsers you want to consume the output of.

This means that two parsers written to consume the same grammar may look quite different, if their authors
had different requirements about how much information to capture in their output [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree).
A "recognizer" that simply validates the source passed to it will have a very different structure from a parser that produces an AST.

FParsec-Pipes replaces all such sequencing combinators with "pipes", so named because they fill the purpose of the `pipe2` family of operators.
The rules for combining parsers with these operators are simple:

* Start the pipe with `%%` followed by the first parser.
* Add additional parsers to the pipe with the `--` operator.
* "Capture" parser outputs in the pipe with the `+.` prefix operator.
* Complete the pipe with `-|>` by supplying a function to combine all the "captured" outputs.

All the operators use `%` to convert their operands to parsers, so you can use raw characters and strings
in the pipeline. Here's the rewritten version of the `Vector3D` parser.
*)

let vector3pipe : Parser<_, unit> =
    %% '(' -- spaces
    -- +.p<float> -- spaces -- ',' -- spaces
    -- +.p<float> -- spaces -- ',' -- spaces
    -- +.p<float> -- spaces -- ')'
    -|> fun x y z -> { X = x; Y = y; Z = z }

(**

Sometimes it's overkill to write a function to combine the captured outputs of a pipe.
Maybe the pipe has no outputs, or only one, or you just need a tuple.

For pipes with no outputs, you can give any value to `-|>` and that value will be returned from the parser on success.
This is like the `>>%` operator in FParsec.

*)

let unitexample : Parser<unit, unit> =
    %% "this pipe" -- "has no" -- "captures" -|> ()

// Always produces the value 1337 on success
let constexample : Parser<int, unit> =
    %% "some text" -|> 1337

(**

For pipes with 1 to 5 outputs, you can automatically combine them into a tuple by using
`-%> auto` as the terminator.

*)

// Parses an int
let auto1example : Parser<_, unit> =
    %% "this pipe" -- "has one" -- +.p<int> -- "capture"
    -%> auto

// Parse a tuple (float, int)
let auto2example : Parser<_, unit> =
    %% "this pipe" -- +.p<float> -- "has two" -- +.p<int> -- "captures"
    -%> auto

(**
### Backtracking with pipes

Sometimes it is necessary to backtrack in a parser.
For example, suppose that at a given place in a file format, it is valid to find either
a date/time in ISO 8601 format, or a Unix timestamp as a 64-bit integer.

Here is one attempt at this:

*)

type Timestamp =
    | UnixTimestamp of int64
    | CalendarTimestamp of DateTime

let unixTimestamp = %% +.p<int64> -|> UnixTimestamp
let calendarTimestamp =
    let digits (count : int) = %% +.(qty.[count] * digit) -|> (String >> Int32.Parse)
    %% +.digits 4 -- '-' -- +.digits 2 -- '-' -- +.digits 2 -- 'T'
    -- +.digits 2 -- ':' -- +.digits 2 -- ':' -- +.digits 2
    -|> fun yyyy mm dd h m s ->
        new DateTime(yyyy, mm, dd, h, m, s) |> CalendarTimestamp

let timestamp : Parser<_, unit> =
    %[calendarTimestamp; unixTimestamp]

(**

The problem with this approach is that, given the Unix timestamp input `1458730894`,
the first part of `calendarTimestamp` will succeed since it successfully parses 4 digits.
It will fail when it encounters `7` as the next character instead of the expected `-`,
but since it's already consumed input and advanced the parser state, the alternative
`unixTimestamp` parser will not be attempted.

The solution is to add backtracking to `calendarTimestamp`. What we want is to ensure that,
until we've seen the first `-`, the whole pipe can be backtracked to the beginning.

Vanilla FParsec provides the `attempt`, `>>?`, `.>>?`, and `.>>.?` combinators for this type of situation.
However, there's no need to abandon the pipe syntax -- it supports backtracking too, with the `?-` and `-?` operators.

The `?-` operator wraps everything to its left in `attempt`.

*)

let calendarTimestampBacktrack1 : Parser<_, unit> =
    let digits (count : int) = %% +.(qty.[count] * digit) -|> (String >> Int32.Parse)
    %% +.digits 4 -- '-' ?- +.digits 2 -- '-' -- +.digits 2 -- 'T'
                      // ^^ Backtrack the whole pipe if anything to the left of this fails.
    -- +.digits 2 -- ':' -- +.digits 2 -- ':' -- +.digits 2
    -|> fun yyyy mm dd h m s ->
        new DateTime(yyyy, mm, dd, h, m, s) |> CalendarTimestamp

(**

The `-?` also wraps everything to its left in `attempt`, but also joins its left and right
together using `.>>.?`, so that if the right side fails _without changing the parser state_,
the pipe will backtrack to the beginning.

This is a bit harder to understand, but can sometimes produce better error messages.

*)

let calendarTimestampBacktrack2 : Parser<_, unit> =
    let digits (count : int) = %% +.(qty.[count] * digit) -|> (String >> Int32.Parse)
    %% +.digits 4 -? '-' -- +.digits 2 -- '-' -- +.digits 2 -- 'T'
                  // ^^ Backtrack the whole pipe if anything to the left of this fails,
                  // or if the parser to the right fails without changing the parser state.
    -- +.digits 2 -- ':' -- +.digits 2 -- ':' -- +.digits 2
    -|> fun yyyy mm dd h m s ->
        new DateTime(yyyy, mm, dd, h, m, s) |> CalendarTimestamp

(**

You can think of `?-` and `-?` as marking the end of ambiguity.

`?-` says "if we've gotten this far, we're definitely looking at something that is *supposed* to
match this parser, and if it doesn't, that's a syntax error."

`-?` is similar, but changes the condition to "if we've gotten this far, *and* the next little bit doesn't *immediately* look wrong".

### Repetition and separation

FParsec includes many combinators for defining parsers like "zero or more of `parserA`" or
"one or more of `parserA` separated by `parserB`". These include `many`, `many1`, `sepBy`, and `sepEndBy`.

FParsec-Pipes uses F#'s slice notation to handle the concept of ranges.
You can slice into `qty` to get a `Range` object, and multiply a parser
(or a value that can be converted to a parser with `%`) by that range.

The resulting parser parses a `System.Collections.Generic.List` (not an F# list) of values.

*)

'a' * qty.[3]    // parse exactly 3 'a's   -- similar to `parray 3 %'a'`
'a' * qty.[1..]  // parse one or more 'a's -- similar to `many1 %'a'`
'a' * qty.[0..]  // parse 0 or more 'a's   -- similar to `many %'a'`
'a' * qty.[..5]  // parse up to 5 'a's
'a' * qty.[3..6] // parse at least 3 and at most 6 'a's
qty.[3] * 'a'    // the range can be on either side of the *

(**

For separation, `/` is also overloaded on ranges.
Another variant, `/.`, allows (but does not require) a separator to appear after the last element.

Mathematically this is of course nonsense, but `/` was chosen because
you can say that the elements are "divided" by their separators, in the
same way that a fence divides pieces of property.

*)

'a' * (qty.[0..] / ',') // parse 0 or more 'a's separated by ','s
                        // similar to sepBy %'a' %','

qty.[0..] / ',' * 'a'   // same as above, just written in a different order
                        // this may be preferable as it doesn't require parentheses

qty.[1..] /. ',' * 'a'  // parse one or more 'a's separated by ','s, with an optional trailing ','
                        // similar to sepEndBy1 %'a' %','

let comma() = %% "," -- spaces -|> ()
let element() = %% 'a' -- spaces -|> ()
in qty.[2..10] / comma() * element() // parse 2 to 10 'a's separated by commas, with whitespace allowed