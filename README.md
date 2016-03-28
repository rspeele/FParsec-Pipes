# FParsec-Pipes

This library is an extension to the FParsec library (http://www.quanttec.com/fparsec/).

Please see the [project page](http://rspeele.github.io/FParsec-Pipes/)
or read the [intro](http://rspeele.github.io/FParsec-Pipes/Intro.html).

FParsec-Pipes does not define new parsers.
Instead, its goal is to make it easier and more readable to define your own parsers.

## Why should I care? Show me some code I can write with this.

```fsharp
let pdatetime =
    let digits (count : int) = %% +.(count, digit) -%> (String >> Int32.Parse)
    %% +.digits 4 -- '-' -- +.digits 2 -- '-' -- +.digits 2 -- 'T'
    -- +.digits 2 -- ':' -- +.digits 2 -- ':' -- +.digits 2
    -%> fun yyyy mm dd h m s ->
        new DateTime(yyyy, mm, dd, h, m, s)
```



