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

// Parses ignored tokens in the stream, requiring at least one.
let ignored1 = skipMany1 ignoredToken
// Parses ignored tokens in the stream, if any.
let ignored = skipMany ignoredToken

let inline (-%) pipe parser = pipe -- ignored -- parser

(**

Next, we have literal values and names, the building blocks of any language.

First we must define a type to hold the parsed values.

*)

type Value =
    | Variable of string
    | IntValue of int64
    | FloatValue of double
    | StringValue of string
    | BooleanValue of bool
    | EnumValue of string
    | ListValue of ResizeArray<Value>
    | ObjectValue of IDictionary<string, Value>

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

Both float and int parsers can be implemented to match the
[spec]
by way of FParsec's `numberLiteral` parser.

We use some extra validation to ensure that zero does not appear before another
digit in the integer part of the literal, as required by the spec.

*)

let numericValue =
    let numberOptions =
        NumberLiteralOptions.AllowMinusSign
        ||| NumberLiteralOptions.AllowExponent
        ||| NumberLiteralOptions.AllowFraction
    let invalidLeadingZero = new Regex(@"-?0[0-9]")
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
    let listElement =
        %% +.value -- ignored -%> auto
    %% '['
    -% +.(listElement * qty.[0..])
    -- ']'
    -%> ListValue

(**

Object values are nearly identical to list values, but include property names.

*)

let objectValue (value : Parser<Value, _>) =
    let objectField =
        %% +.name
        -% ':'
        -% +.value
        -- ignored
        -%> auto
    %% '{'
    -- ignored
    -- +.(objectField * qty.[0..])
    -- '}'
    -%> (dict >> ObjectValue)

(**

Now that we have all the different types of values defined, they can be unified into
one with a recursive parser definition.

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

Now that we have values, let's work on building up to definitions.
Step 1 is to define the types we'll be parsing.

*)

// An argument is a named value.

type Argument =
    {
        ArgumentName : string
        ArgumentValue : Value
    }

let opts parser =
    %[
        parser
        preturn (new ResizeArray<_>())
    ]

let arguments =
    let argument =
        %% +.name
        -% ':'
        -% +.value -- ignored
        -%> fun name value ->
            { ArgumentName = name; ArgumentValue = value }
    %% '(' -% +.(argument * qty.[1..]) -- ')'
    -%> auto

type Directive =
    {
        DirectiveName : string
        Arguments : Argument ResizeArray
    }

let directives =
    let directive =
        %% '@' -- +.name
        -% +.opts arguments -- ignored
        -%> fun name args ->
            { DirectiveName = name; Arguments = args }
    directive * qty.[1..]

type FragmentName = string

let fragmentName =
    name >>= fun name ->
        if name = "on" then fail "fragment name may not be `on`"
        else preturn name

type FragmentSpread =
    {
        FragmentName : FragmentName
        Directives : Directive ResizeArray
    }

let fragmentSpread =
    %% "..."
    -% +.name
    -% +.opts directives
    -%> fun name dirs ->
        { FragmentName = name; Directives = dirs }

type TypeName = string

type Field =
    {
        Alias : string option
        FieldName : string
        Arguments : Argument ResizeArray
        Directives : Directive ResizeArray
        Selections : Selection ResizeArray
    }
and Selection =
    | FieldSelection of Field
    | FragmentSpreadSelection of FragmentSpread
    | InlineFragmentSelection of InlineFragment
and InlineFragment =
    {
        TypeCondition : TypeName option
        Directives : Directive ResizeArray
        Selections : Selection ResizeArray
    }

let field selections : Parser<_, _> =
    let alias = %% +.name -- ignored -? ':' -- ignored -%> auto
    %% +.(alias * zeroOrOne)
    -- +.name
    -% +.opts arguments
    -% +.opts directives
    -% +.opts selections
    -%> fun alias name args dirs sels ->
        {
            Alias = alias
            FieldName = name
            Arguments = args
            Directives = dirs
            Selections = sels
        }

let typeCondition =
    %% "on" -% +.name -%> auto

let inlineFragment selections =
    %% "..."
    -% +.(typeCondition * zeroOrOne)
    -% +.opts directives
    -% +.selections
    -%> fun typeCond dirs sels ->
        {
            TypeCondition = typeCond
            Directives = dirs
            Selections = sels
        }

let selections = precursive <| fun selections ->
    let selection =
        %[
            %% +.field selections -%> FieldSelection
            %% +.fragmentSpread -%> FragmentSpreadSelection
            %% +.inlineFragment selections -%> InlineFragmentSelection
        ]
    %% '{' -% +.(selection * qty.[1..]) -% '}' -%> auto

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
        %% '[' -% +.(typeDescription * qty.[1..]) -% ']' -%> ListType
    let namedType =
        %% +.name -%> NamedType
    %[
        listType
        namedType
    ]

let typeDescription = precursive <| fun typeDescription ->
    %% +.coreTypeDescription typeDescription
    -% +.('!' * zeroOrOne)
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
    -% +.value
    -%> auto

let variableDefinition =
    %% +.variableName
    -% ':'
    -% +.typeDescription
    -% +.(defaultValue * zeroOrOne)
    -- ignored
    -%> fun variable ty defaultVal ->
        {
            VariableName = variable
            Type = ty
            DefaultValue = defaultVal
        }

let variableDefinitions =
    %% '('
    -% +.(variableDefinition * qty.[1..])
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
        VariableDefinitions : VariableDefinition ResizeArray
        Directives : Directive ResizeArray
        Selections : Selection ResizeArray
    }

let longhandOperation =
    %% +.operationType
    -% +.(name * zeroOrOne)
    -% +.opts variableDefinitions
    -% +.opts directives
    -% +.opts selections
    -%> fun ty name varDefs dirs sels ->
        {
            Type = ty
            Name = name
            VariableDefinitions = varDefs
            Directives = dirs
            Selections = sels
        }

type Operation =
    | ShorthandOperation of Selection ResizeArray
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
        Directives : Directive ResizeArray
        Selections : Selection ResizeArray
    }

let fragment =
    %% "fragment"
    -% +.fragmentName
    -% +.typeCondition
    -% +.opts directives
    -% +.selections
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

(**
*)