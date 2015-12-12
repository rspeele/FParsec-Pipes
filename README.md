# FParsec-Pipes

This library is an extension to the FParsec library (http://www.quanttec.com/fparsec/).

FParsec-Pipes does not define new parsers.
Instead, its goal is to make it easier and more readable to define your own parsers,
in particular those that chain multiple parsers together and combine their outputs into a single value.
It accomplishes this by defining a new set of operators for gluing parsers together and applying functions to their outputs.

When using FParsec-Pipes, you don't have to think very hard about what operators to use to 

## Why should I care? Show me some code I can write with this.

```fsharp
// Parses a 3-element vector of integers like "(1, 2, 3)".
let vec =
    let comma =
        |-- spaces
        |-- ','
        |-- spaces
        |=> ()
    pipe
    |-- '('
    |-- spaces
    |-+ parse<int>
    |-- comma
    |-+ parse<int>
    |-- comma
    |-+ parse<int>
    |-- spaces
    |-- ')'
    |=> fun x y z -> (x, y, z)
// vec : Parser<int * int * int, 'u>
```



