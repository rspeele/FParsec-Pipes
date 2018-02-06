(*** hide ***)

#r @"../packages/FParsec.1.0.2/lib/portable-net45+netcore45+wpa81+wp8/FParsecCS.dll"
#r @"../packages/FParsec.1.0.2/lib/portable-net45+netcore45+wpa81+wp8/FParsec.dll"
#r @"../FParsec-Pipes/bin/Debug/netstandard2.0/FParsec-Pipes.dll"
#nowarn "193"
open System
open FParsec
open FParsec.Pipes

(**

# FParsec-Pipes reference

## Default Parsers

### %

The default parser for a given value is `%value`.
This is implicitly used by several other FParsec-Pipes operators.
**For the rest of this document, the phrase "parserish value" will refer to a value that
either is a parser or can be converted to one using `%`.**
Here is the list of what `%` means based on the type given to it.
Note that when given a list of values, `%` applies itself to each value in the list then passes the result to `choice`.

| Type                           | Expression              | Parser Type             | Equivalent Function |
+--------------------------------+-------------------------+-------------------------+---------------------+
| `Parser<'a, 'u>`               | `preturn 1`             | `Parser<'a, 'u>`        | `id` |
| `char`                         | `'a'`                   | `Parser<char, 'u>`      | `pchar` |
| `string`                       | `"str"`                 | `Parser<string, 'u>`    | `pstring` |
| `'a list`                      | `[ 'a'; 'b' ]`          | `Parser<'b, 'u>`        | `choice << List.map (%)` |
| `CaseInsensitive<char>`        | `ci 'a'`                | `Parser<char, 'u>`      | `pcharCI` |
| `CaseInsensitive<string>`      | `ci "str"`              | `Parser<string, 'u>`    | `pstringCI` |
| `DefaultParserOf<char>`        | `p<char>`               | `Parser<char, 'u>`      | `anyChar` |
| `DefaultParserOf<float>`       | `p<float>`              | `Parser<float, 'u>`     | `pfloat` |
| `DefaultParserOf<int8>`        | `p<int8>`               | `Parser<int8, 'u>`      | `pint8` |
| `DefaultParserOf<int16>`       | `p<int16>`              | `Parser<int16, 'u>`     | `pint16` |
| `DefaultParserOf<int32>`       | `p<int32>`              | `Parser<int32, 'u>`     | `pint32` |
| `DefaultParserOf<int64>`       | `p<int64>`              | `Parser<int64, 'u>`     | `pint64` |
| `DefaultParserOf<uint8>`       | `p<uint8>`              | `Parser<uint8, 'u>`     | `puint8`  |
| `DefaultParserOf<uint16>`      | `p<uint16>`             | `Parser<uint16, 'u>`    | `puint16` |
| `DefaultParserOf<uint32>`      | `p<uint32>`             | `Parser<uint32, 'u>`    | `puint32` |
| `DefaultParserOf<uint64>`      | `p<uint64>`             | `Parser<uint64, 'u>`    | `puint64` |
| `DefaultParserOf<Position>`    | `p<Position>`           | `Parser<Position, 'u>`  | `getPosition` |
| `DefaultParserOf<'a>`          | `p<'a>`                 | `Parser<'a, 'u>`        | `'a.DefaultParser` |

## Pipes

### pipe

`pipe` begins a pipeline. This is not useful on its own.

### %%

`%% parserish` begins a pipeline with a parserish value.
It is equivalent to `pipe -- parserish`.

### --

`myPipe -- parserish` adds a parserish value to a pipeline.

### ?-

`myPipe ?- parserish` adds a parserish value to a pipeline, backtracking the entire pipeline if
anything within `myPipe` fails.

### -?

`myPipe -? parserish` adds a parserish value to a pipeline, backtracking the entire piplime
if `myPipe` fails or if `parserish` and following values in the pipeline fail without consuming input.

### +.

`+. parserish` marks a parserish value as captured.
If added to a pipeline, this captured value will need to be consumed by the function passed to `-|>`.

### -|>

`myPipe -|> myFunction` terminates a pipeline with a function consuming the captured outputs of the pipeline.
If the pipeline has no outputs (no captured values), then `myFunction` can be of any type, and will be returned
on a successful parse (as if used with `preturn`).

### -%> auto

`myPipe -%> auto` terminates a pipeline which has 1 to 5 captured outputs with a function combining them
into a tuple of the appropriate arity.

## Repetition

### qty

`qty` is an object which can be indexed or sliced into to get a `Range`.

### qty.[min..max]

`qty.[min..max]` indicates that the associated parser should be consumed at least `min` and at most `max` times.

### qty.[..max]

`qty.[..max]` indicates that the associated parser should be consumed at least 0 and at most `max` times.

### qty.[min..]

`qty.[min..]` indicates that the associated parser should be consumed at least `min` times.

### qty.[n]

`qty.[n]` indicates that the associated parser should be consumed exactly `n` times.
The resulting parser will return an array, not a `ResizeArray`.

### Range * parserish

`range * parserish` consumes `parserish` `range` times and returns the result as a ResizeArray.

### parserish * Range

`parserish * range` is equivalent to `range * parserish`.

### Range / parserish

`range / parserish` indicates that the parser that will be consumed `range` times should be separated by `parserish`.

### Range /. parserish

`range / parserish` indicates that the parser that will be consumed `range` times should be separated by `parserish`,
and that a trailing `parserish` may occur at the end of the list.

*)



