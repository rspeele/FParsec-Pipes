(*** hide ***)

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

// Parses `count` digits and returns the result as an integer.
let digits (count : int) =
    !+ (count, digit) -|> (String >> Int32.Parse)

// Parses a yyyy-mm-dd date and returns the result as a tuple of 3 integers.
let date =
    !+ digits(4) -- '-' -+ digits(2) -- '-' -+ digits(2)
    -|> fun yyyy mm dd -> (yyyy, mm, dd)

// Parses a hh:mm:ss time and returns the result as a tuple of 3 integers.
let time =
    !+ digits(2) -- ':' -+ digits(2) -- ':' -+ digits(2)
    -|> fun hh mm ss -> (hh, mm, ss)

// Parses a DateTime in the format yyyy-mm-ddThh:mm:ss
let datetime : Parser<_, unit> =
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

The value you pass to `%` can also be of type `DefaultParserOf<'a>`, which you
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

Let's see how the same parser would be written with those operators - first, with verbose comments.
*)

let vector3 : Parser<_, unit> =
    let comma =
        pipe      // `pipe` starts a pipeline of parsers.
        -- spaces // The `--` operator adds a parser to the pipeline, ignoring its output.
        -- ','    // `--` automatically applies `%` to the value on its right side.
        -- spaces
        -|> ()    // `-|>` ends the pipeline, turning it into a parser.
                  // In this case, since the pipeline has no captures (all parsers are ignored)
                  // we just give it a value for the parser to return.

    pipe           // For the vector parser, start with `pipe` again (as always).
    -- '('          
    -- spaces
    -+ auto<float> // The `-+` operator is like `--`, but captures the output of the parser.
    -- comma
    -+ auto<float> // You can chain together as many parsers with `--` and `-+` as you like.
    -- comma
    -+ auto<float> // In this case we have three captures.
    -- spaces
    -- ')'
    // As before, we use `-|>` to end the pipeline and get a parser.
    // This time, though, we need to supply a function that takes our three captures
    // and returns the output type of our parser. This is the same function
    // that we gave to `pipe3` in the earlier example.
    -|> fun x y z -> { X = x; Y = y; Z = z }

(**

Now to write it more concisely. Putting more than one parser per line makes it fairly compact.

*)

let vector3short : Parser<_, unit> =
    let comma = pipe -- spaces -- ',' -- spaces -|> ()
    pipe -- '(' -- spaces
    -+ auto<float> -- comma
    -+ auto<float> -- comma
    -+ auto<float> -- spaces -- ')'
    -|> fun x y z -> { X = x; Y = y; Z = z }

(**

Every pipeline starts with `pipe -- x` or, if the first parser needs to be captured, `pipe -+ x`.
These can be replaced with the prefix operators `!-` and `!+` respectively.
`!- x` is equivalent to `pipe -- x`.

Also, at this point the amount of clutter in the parser is low enough that it doesn't look
too bad to just drop `spaces -- ',' -- spaces` in directly instead of binding a separate `comma` parser.

*)

let vector3shorter : Parser<_, unit> =
    !- '(' -- spaces
    -+ auto<float> -- spaces -- ',' -- spaces
    -+ auto<float> -- spaces -- ',' -- spaces
    -+ auto<float> -- spaces -- ')'
    -|> fun x y z -> { X = x; Y = y; Z = z }

(**

At this point you can glue together most parsers just using these operators. There are a few nice things about this:

* To see what tokens get parsed, you always just read left-to-right and top-to-bottom.

* You can rapidly find which parsers contributed the inputs to the function at the end of the pipe by looking for `+` signs.

* The way to structure a sequence of parsers is obvious. You can always start by writing a parser that ignores all its constituent parsers' outputs.
  If you need to capture the output of some of the parsers, just change the operator preceding those parsers to end with a `+` and update
  the function at the end of the pipeline to take the resulting additional parameter.

Basically, the promise is that if you have `parserA` followed by `parserB`, you can _always_ express that with
the same code structure (a binary operator between the two) -- whether you care about their outputs or not.

One question is: what if you need to add backtracking?
In many cases, this can be avoided altogether, and this is the best approach for performance reasons.

However, suppose that you are parsing a language like C#, in which "from" could mean something very different in two expressions that
could easily appear in the same context:

* `from` as a variable name, as in `from.ToString()`.
* `from` as the keyword which begins a LINQ query expression, as in `from x in new[] { 1, 2, 3 } select x * x`.

In this case, you will most likely want to attempt the more complex second possibility, and, if it does not pan out,
backtrack to the start of "from" before parsing it as a variable identifier. This isn't the only way to do this -- you can still
parse these expressions without backtracking -- but it's the clearest way to code it and thus preferred barring performance constraints.

You might write something like this simplified example. Of course, a real parser would need to include a full C# AST,
use the real rules for identifiers, and so on, but the point here is just to demonstrate backtracking, so let's
pretend all we care about is distinguishing variable names from LINQ queries.

*)

type WhichExpressionIsIt =
    | VariableName
    | LinqQuery

// Not the real identifier name rules, but ignore that.
let variableName =
    !- (many1Satisfy (fun c -> isDigit c || isLetter c || c = '_')) -|> VariableName

let linqQuery =
    !- "from" -- spaces1 -- variableName -- spaces1 -- "in" -|> LinqQuery

let whichExpr : Parser<_, unit> =
    %[
        // choice between a...
        linqQuery
        // or a...
        variableName
    ]

(**

This won't work. Given the input text `"from . ToString()"`,
the first choice will _start to succeed_ by parsing the string `"from"` and some spaces.

It will then fail by reading a `'.'` instead of a valid `variableName` character, but by then it's too late.
The start of the string has already been consumed.

The solution to this is to add backtracking to the first parser, and for that, the critical question to ask yourself is:

"At what point am I _certain_ that this is the right parser to be applying?"

In this case, having read the string `"from"`, you don't know that this is going to be a LINQ query.
Having read the following whitespace, you _still_ don't know that this is going to be a LINQ query.
But once you've read another variable name, you can be confident that this is either a LINQ query (`from variableName in ...`) or a syntax error.

The modification to make the parser is very simple:

*)

let linqQueryBacktracking =
    !- "from" -- spaces1 -- variableName ?- spaces1 -- "in" -|> LinqQuery

(**

Did you spot it? Immediately after the `variableName`, `--` has been replaced with `?-`.

Here's what that means: whenever you have `?-` or `?+` instead of `--` or `-+`,
_the entire chunk of the pipeline prior to that operator_ will be wrapped in `attempt` ,
meaning that if any of its constituent parsers fail, the input stream will backtrack to the start
of the pipeline.

*)

