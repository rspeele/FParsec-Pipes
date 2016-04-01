(*** hide ***)

#r @"bin/debug/FParsecCS.dll"
#r @"bin/debug/FParsec.dll"
#r @"bin/debug/FParsec-Pipes.dll"
#nowarn "193"
open System
open System.Collections.Generic
open System.Globalization
open System.Text.RegularExpressions
open FParsec
open FParsec.Pipes

(**

This example parses Facebook's
[GraphQL](https://facebook.github.io/graphql/)
data query language.

It is based on the grammar summary in
[appendix B](https://facebook.github.io/graphql/#sec-Appendix-Grammar-Summary)
of the draft RFC specification.

We begin with ignored tokens. The spec goes into detail here
which it would be counterproductive to try to reproduce in 1-for-1 detail,
since FParsec's `CharStream` class already handles things like normalizing newlines
and skipping over Unicode byte order marks.

*)

let comment =
    %% '#' -- restOfLine false -%> ()

let isIgnoredCharacter c =
    c = '\t'
    || c = ' '
    || c = ','

// Parses a run of ignored tokens of similar type.
let ignoredToken =
    %[
        // Either a run of spaces/commas
        skipMany1Satisfy isIgnoredCharacter
        // A newline
        skipNewline
        // Or a line comment
        comment
    ]

// Parses ignored tokens in the stream, if any.
let ignored = skipMany ignoredToken

(**

We'll frequently be allowing ignored tokens in between other parsers,
so let's define an inline operator to make this easier.

*)

/// Chain `parser` onto `pipe` with ignored tokens allowed to precede it.
let inline (-..-) pipe parser = pipe -- ignored -- parser

(**

Next, we have literal values and names, the building blocks of any language.

First we must define a type to hold the parsed values. A practical parser should always
include source information (at least line numbers) in the parsed AST, so that code
consuming the AST can can produce useful warnings or errors when it encounters constructs
that are semantically invalid, like type mismatches or unresolved variable names.

We'll define our own types to carry this information, so that callers don't need to directly reference
FParsec's `Position` type. We'll also use helpers to wrap unadorned parsers with source information.

*)

type SourcePosition =
    {
        Index : int64
        Line : int64
        Column : int64
    }

type SourceInfo =
    {
        StartPosition : SourcePosition
        EndPosition : SourcePosition
    }

// Translates from FParsec's position type to our own.
let sourcePosition =
    %% +.p<Position>
    -%> fun pos -> { Index = pos.Index; Line = pos.Line; Column = pos.Column }

type WithSource<'a> =
    {
        Source : SourceInfo
        Value : 'a
    }

// Wraps any parser with source information.
let withSource (parser : Parser<'a, unit>) =
    %% +.sourcePosition
    -- +.parser
    -- +.sourcePosition
    -%> fun startPos value endPos ->
        {
            Source = { StartPosition = startPos; EndPosition = endPos }
            Value = value
        }

// Most of the time when we need to put source info on an AST element
// it's because it's within a list, therefore the SourceInfo of its containing
// element will be too general to direct the user to the problem location.
// Because of how common this is, we define the type alias `ListWithSource` for a list of
// elements with source info on each one.
type ListWithSource<'a> = ResizeArray<WithSource<'a>>

type Value =
    | Variable of string
    | IntValue of int64
    | FloatValue of double
    | StringValue of string
    | BooleanValue of bool
    | EnumValue of string
    | ListValue of Value ListWithSource
    | ObjectValue of IDictionary<string, Value WithSource>

(**

Several different parsers consume identifiers.
An identifier could be a variable name, an enum value, or field name in an object.
The rules for these names are simple enough. They must begin with an underscore
or alphanumeric character. After the first character, digits are also permitted.

> Name :: /[_A-Za-z][_0-9A-Za-z]*/

*)

// Parses an identifier name.
let name =
    let isInitial c =
        c = '_'
        || c >= 'A' && c <= 'Z'
        || c >= 'a' && c <= 'z'
    let isFollowing c =
        isInitial c
        || c >= '0' && c <= '9'
    many1Satisfy2 isInitial isFollowing

(**

Variables are simply names prefixed with a "$" sign.
We will not include the "$" sign in the AST, considering it
to be purely syntax rather than a part of the name.

*)

let variableName = 
    %% '$' -- +.name -%> auto

let variable =
    %% +.variableName -%> Variable

(**

Both float and int parsers can be implemented to match the spec
by way of FParsec's `numberLiteral` parser.

We use some extra validation to ensure that zero does not appear before another
digit in the integer part of the literal, as required by the spec.

*)

let numericValue =
    let numberOptions =
        NumberLiteralOptions.AllowMinusSign
        ||| NumberLiteralOptions.AllowExponent
        ||| NumberLiteralOptions.AllowFraction
    let invalidLeadingZero = new Regex(@"^-?0[0-9]")
    numberLiteral numberOptions "numeric literal"
    >>= fun literal ->
        if invalidLeadingZero.IsMatch(literal.String) then
            literal.String
            |> sprintf "Non-zero numeric literal (%s) may not start with a 0"
            |> fail
        else if literal.IsInteger then
            literal.String |> Int64.Parse |> IntValue |> preturn
        else
            literal.String |> Double.Parse |> FloatValue |> preturn

(**

String literals are comparable to those in JavaScript.

Strings consist of runs of 0 or more normal characters
separated by escaped characters. FParsec includes a `stringSepBy`
to parse this type of sequence efficiently.

*)

let stringValue =
    let isRegularCharacter c =
        c <> '"'
        && c <> '\''
        && c <> '\n'
    let regularCharacters = manySatisfy isRegularCharacter
    let escapedCharacter =
        let unicode (hex4 : char array) =
            char <| Int32.Parse(new String(hex4), NumberStyles.HexNumber)
        %% '\\'
        -- +.[
            %% 'u' -- +.(4, hex) -%> unicode
            % '"'
            % '\\'
            %% 'b' -%> '\b'
            %% 'f' -%> '\x0C' // form feed
            %% 'n' -%> '\n'
            %% 'r' -%> '\r'
            %% 't' -%> '\t'
        ]
        -%> auto
    let escapedCharacters = many1Chars escapedCharacter
    %% '"'
    -- +.stringsSepBy regularCharacters escapedCharacters
    -- '"'
    -%> StringValue

(**

Boolean literals are of course trivial to parse.
Interestingly, GraphQL has no null literal --
if it did, it would be parsed much the same way.

*)

let booleanValue =
    %[
        %% "true" -%> BooleanValue true
        %% "false" -%> BooleanValue false
    ]

(**

Enum values can be any name that is not a boolean value or null.
We can avoid parsing boolean values by prioritizing them ahead of enum
values in the parser. Null must be manually checked for, however, since
there is no null literal.

*)

let enumValue =
    name >>= function
    | "null" -> fail "null is not a legal enum value name"
    | x -> preturn (EnumValue x)

(**

In order to define list values, we need a complete value parser.
Since we haven't defined the parser unifying all values yet,
we'll take it as an argument.

*)

let listValue (value : Parser<Value, _>) =
    %% '['
    -..- +.(qty.[0..] /.ignored * withSource value)
    -- ']'
    -%> ListValue

(**

Object values are nearly identical to list values, but include property names.

*)

let objectValue (value : Parser<Value, _>) =
    let sourceValue = withSource value
    let objectField =
        %% +.name
        -..- ':'
        -..- +.sourceValue
        -%> auto
    %% '{'
    -..- +.(qty.[0..] /. ignored * objectField)
    -- '}'
    -%> (dict >> ObjectValue)

(**

Now that we have all the different types of values defined, they can be unified into
a general value parser with a recursive definition.

*)

let value = precursive <| fun value ->
    %[
        variable
        numericValue
        stringValue
        booleanValue
        enumValue
        listValue value
        objectValue value
    ]

(**

The spec also defines a "const" variant of the value parser, which excludes variables
and lists/objects containing variables. Because our list and object parsers are parameterized
this is trivial to implement.

*)

let valueConst = precursive <| fun valueConst ->
    %[
        numericValue
        stringValue
        booleanValue
        enumValue
        listValue valueConst
        objectValue valueConst
    ]

(**

Now that we have values, let's keep building up to full queries.

An [argument](https://facebook.github.io/graphql/#sec-Language.Arguments)
is a named value. It is found in an argument list, which looks like this:
    (arg1: value1, arg2: value2)
Remember, however, that the commas are entirely optional, since they are ignored tokens.

*)

/// Represents an `argName: argValue` pair
type Argument =
    {
        ArgumentName : string
        ArgumentValue : Value
    }

/// Parses an argument list wrapped in parentheses. Per the GraphQL spec,
/// this list must contain at least one argument.
let arguments =
    let argument =
        %% +.name
        -..- ':'
        -..- +.value
        -%> fun name value ->
            { ArgumentName = name; ArgumentValue = value }
    %% '(' -..- +.(qty.[1..] /. ignored * withSource argument) -- ')'
    -%> auto

(**

The
[spec on directives](https://facebook.github.io/graphql/#sec-Language.Directives)
explains that they can be used to describe additional information for fields, fragments, and operations.

A directive looks like this:
    @dirName
Or, with arguments:
    @dirName(arg1: value1, arg2: value2)

*)
type Directive =
    {
        DirectiveName : string
        Arguments : Argument ListWithSource
    }

/// Runs the given list parser, which parses a list of one or more elements,
/// or returns an empty list. This is useful when a list is optional but must
/// contain at least one item if it is present, which is common in GraphQL.
let optionalMany parser =
    %[
        parser
        preturn (new ResizeArray<_>())
    ]

/// Parses a list of 0 or more directives.
let directives =
    let directive =
        %% '@' -- +.name
        -..- +.optionalMany arguments
        -%> fun name args ->
            { DirectiveName = name; Arguments = args }
    qty.[0..] /. ignored * withSource directive

(**

[Fragments](https://facebook.github.io/graphql/#sec-Language.Fragments)
define reusable selections of fields.

They can be given names, which are allowed to be anything except "on".
We'll add this validation to our usual `name` parser.

*)

type FragmentName = string

let fragmentName =
    name >>= fun name ->
        if name = "on" then fail "fragment name may not be `on`"
        else preturn name

(**

A fragment spread references a named fragment, bringing its fields into the current selection scope.

It looks like:
    ...fragmentName
Or, with directives:
    ...fragmentName @dir1 @dir2(arg1:value1)

*)

type FragmentSpread =
    {
        FragmentName : FragmentName
        Directives : Directive ListWithSource
    }

let fragmentSpread =
    %% "..."
    -..- +.name
    -..- +.optionalMany directives
    -%> fun name dirs ->
        { FragmentName = name; Directives = dirs }

(**

*)

type TypeName = string

type Field =
    {
        Alias : string option
        FieldName : string
        Arguments : Argument ListWithSource
        Directives : Directive ListWithSource
        Selections : Selection ListWithSource
    }
and Selection =
    | FieldSelection of Field
    | FragmentSpreadSelection of FragmentSpread
    | InlineFragmentSelection of InlineFragment
and InlineFragment =
    {
        TypeCondition : TypeName option
        Directives : Directive ListWithSource
        Selections : Selection ListWithSource
    }

let field selections : Parser<_, _> =
    let alias = %% +.name -- ignored -? ':' -- ignored -%> auto
    %% +.(alias * zeroOrOne)
    -- +.name
    -..- +.optionalMany arguments
    -..- +.directives
    -..- +.optionalMany selections
    -%> fun alias name args dirs sels ->
        {
            Alias = alias
            FieldName = name
            Arguments = args
            Directives = dirs
            Selections = sels
        }

let typeCondition =
    %% "on" -..- +.name -%> auto

let inlineFragment selections =
    %% "..."
    -..- +.(typeCondition * zeroOrOne)
    -..- +.directives
    -..- +.selections
    -%> fun typeCond dirs sels ->
        {
            TypeCondition = typeCond
            Directives = dirs
            Selections = sels
        }

let selections = precursive <| fun selections ->
    let selection =
        %% +.[
            %% +.field selections -%> FieldSelection
            %% +.fragmentSpread -%> FragmentSpreadSelection
            %% +.inlineFragment selections -%> InlineFragmentSelection
        ] -- ignored -%> auto
    %% '{' -..- +.(qty.[1..] /. ignored * withSource selection) -- '}' -%> auto

type CoreTypeDescription =
    | NamedType of TypeName
    | ListType of TypeDescription ResizeArray
and TypeDescription =
    {
        Type : CoreTypeDescription
        Nullable : bool
    }

let coreTypeDescription typeDescription =
    let listType =
        %% '[' -..- +.(qty.[1..] * typeDescription) -..- ']' -%> ListType
    let namedType =
        %% +.name -%> NamedType
    %[
        listType
        namedType
    ]

let typeDescription = precursive <| fun typeDescription ->
    %% +.coreTypeDescription typeDescription
    -..- +.('!' * zeroOrOne)
    -- ignored
    -%> fun desc bang ->
        {
            Type = desc
            Nullable = bang <> None
        }

type VariableDefinition =
    {
        VariableName : string
        Type : TypeDescription
        DefaultValue : Value option
    }

let defaultValue =
    %% '='
    -..- +.valueConst
    -%> auto

let variableDefinition =
    %% +.variableName
    -..- ':'
    -..- +.typeDescription
    -..- +.(defaultValue * zeroOrOne)
    -%> fun variable ty defaultVal ->
        {
            VariableName = variable
            Type = ty
            DefaultValue = defaultVal
        }

let variableDefinitions =
    %% '('
    -..- +.(qty.[1..] /. ignored * withSource variableDefinition)
    -- ')'
    -%> auto

type OperationType =
    | Query
    | Mutation

let operationType =
    %[
        %% "query" -%> Query
        %% "mutation" -%> Mutation
    ]

type LonghandOperation =
    {
        Type : OperationType
        Name : string option
        VariableDefinitions : VariableDefinition ListWithSource
        Directives : Directive ListWithSource
        Selections : Selection ListWithSource
    }

let longhandOperation =
    %% +.operationType
    -..- +.(name * zeroOrOne)
    -..- +.optionalMany variableDefinitions
    -..- +.optionalMany directives
    -..- +.optionalMany selections
    -%> fun ty name varDefs dirs sels ->
        {
            Type = ty
            Name = name
            VariableDefinitions = varDefs
            Directives = dirs
            Selections = sels
        }

type Operation =
    | ShorthandOperation of Selection ListWithSource
    | LonghandOperation of LonghandOperation

let operation =
    %[
        %% +.selections -%> ShorthandOperation
        %% +.longhandOperation -%> LonghandOperation
    ]

type Fragment =
    {
        FragmentName : string
        TypeCondition : TypeName
        Directives : Directive ListWithSource
        Selections : Selection ListWithSource
    }

let fragment =
    %% "fragment"
    -..- +.fragmentName
    -..- +.typeCondition
    -..- +.optionalMany directives
    -..- +.selections
    -%> fun name typeCond dirs sels ->
        {
            FragmentName = name
            TypeCondition = typeCond
            Directives = dirs
            Selections = sels
        }

type Definition =
    | OperationDefinition of Operation
    | FragmentDefinition of Fragment

let definition : Parser<Definition, unit> =
    %[
        %% +.operation -%> OperationDefinition
        %% +.fragment -%> FragmentDefinition
    ]

let definitions =
    %% ignored
    -- +.(qty.[0..] /. ignored * withSource definition)
    -%> auto

(**
*)