module Examples.SQLite.SQLiteParser
open System
open System.Collections.Generic
open System.Globalization
open FParsec
open FParsec.Pipes
open FParsec.Pipes.Precedence

(**

Here is an example parser for SQLite queries based on the
[syntax diagrams](https://www.sqlite.org/syntaxdiagrams.html)
in the SQLite documentation.

The syntax diagrams, like many language grammars, define the language
from the top-down. In F# source, variables must be assigned before they
are used, so this parser is written in reverse order, constructing the
grammar starting with its smallest building blocks.

We will begin with comments and whitespace.

*)

/// A line comment begins with -- and continues through the end of the line.
let lineComment =
    %% "--" -- restOfLine true -|> ()

/// A block comment begins with /* and continues until a trailing */ is found.
/// Nested block comments are not allowed, so additional /* tokens found
/// after the first are ignored.
let blockComment =
    %% "/*" -- skipCharsTillString "*/" true Int32.MaxValue -|> ()

/// Where whitespace is expected, it can be one of...
let whitespaceUnit =
    %[
        lineComment // a line comment
        blockComment // a block comment
        spaces1 // one or more whitespace characters
    ]

/// Optional whitespace: 0 or more whitespace units
let ws = skipMany whitespaceUnit

/// Add optional trailing whitespace to a parser.
let inline tws parser = %parser .>> ws

/// Required whitespace: 1 or more whitespace units
let ws1 = skipMany1 whitespaceUnit

(**

The syntax diagrams in the SQLite documentation do not explicitly define what a
valid identifier name is. However, we can infer some rules from the
[keyword documentation](https://sqlite.org/lang_keywords.html).

For the sake of both simplicity and strictness, we will not support the "bent rules"
described regarding allowing string literals to appear as identifier names and vice-versa.

*)

/// A name wrapped in double quotes (standard SQL).
let quotedName =
    let escapedQuote =
        %% "\"\"" -|> "\"" // A pair of double quotes escapes a double quote character
    let regularChars =
        many1Satisfy ((<>) '"') // Any run of non-quote characters is literal
    %% '"' -- +.([regularChars; escapedQuote] * qty.[0..]) -- '"'
    -|> String.Concat // Glue together the parts of the string

/// A name wrapped in square brackets (T-SQL style).
let bracketedName =
    let escapedBracket =
        %% "]]" -|> "]" // A pair of right brackets escapes a right bracket character
    let regularChars =
        many1Satisfy ((<>) ']') // Any run of non-bracket characters is literal
    %% '[' -- +.([regularChars; escapedBracket] * qty.[0..]) -- ']'
    -|> String.Concat

/// A name wrapped in backticks (MySQL style)
let backtickedName =
    let escapedTick =
        %% "``" -|> "`" // A pair of backticks escapes a backtick character
    let regularChars =
        many1Satisfy ((<>) '`') // Any run of non-backtick characters is literal
    %% '`' -- +.([regularChars; escapedTick] * qty.[0..]) -- '`'
    -|> String.Concat

(**

For unquoted names, we should ensure that a reserved keyword is not used.
This list of reserved keywords was obtained by starting with the list
[here](https://www.sqlite.org/lang_keywords.html) and removing those for which
my local copy of SQLite accepted the query "SELECT * from X AS KEYWORD".

*)

let sqlKeywords =
    [
        "ADD"; "ALL"; "ALTER";
        "AND"; "AS";
        "BETWEEN"; "CASE"; "CHECK"; "COLLATE";
        "COMMIT"; "CONFLICT"; "CONSTRAINT"; "CREATE"; "CROSS";
        "DEFAULT"; "DEFERRABLE"; "DELETE";
        "DISTINCT"; "DROP"; "ELSE"; "ESCAPE"; "EXCEPT";
        "EXISTS"; "FOREIGN"; "FROM";
        "FULL"; "GLOB"; "GROUP"; "HAVING"; "IN";
        "INNER"; "INSERT";
        "INTERSECT"; "INTO"; "IS"; "ISNULL"; "JOIN"; "LEFT";
        "LIMIT"; "NATURAL"; "NOT"; "NOTNULL"; "NULL";
        "ON"; "OR"; "ORDER"; "OUTER"; "PRIMARY";
        "REFERENCES";
        "RIGHT";
        "SELECT"; "SET"; "TABLE"; "THEN";
        "TO"; "TRANSACTION"; "UNION"; "UNIQUE"; "UPDATE"; "USING";
        "VALUES"; "WHEN"; "WHERE";
        // Note: we don't include TEMP in this list because it is a schema name.
    ] |> fun kws ->
        HashSet<string>(kws, StringComparer.OrdinalIgnoreCase)
        // Since SQL is case-insensitive, be sure to ignore case
        // in this hash set.

let isInitialIdentifierCharacter c =
    c = '_'
    || c >= 'a' && c <= 'z'
    || c >= 'A' && c <= 'Z'

let isFollowingIdentifierCharacter c =
    isInitialIdentifierCharacter c
    || c >= '0' && c <= '9'
    || c = '$'

let unquotedNameOrKeyword =
    many1Satisfy2 isInitialIdentifierCharacter isFollowingIdentifierCharacter

/// A plain, unquoted name.
let unquotedName =
    unquotedNameOrKeyword >>=? fun ident ->
        if sqlKeywords.Contains(ident) then
            fail (sprintf "Reserved keyword %s used as name" ident)
        else
            preturn ident

(**

A name may appear in any of the given forms.

*)

let name =
    %[
        quotedName
        bracketedName
        backtickedName
        unquotedName
    ]

(**

String literals can be used as names in some places. They are similar in implementation to quoted identifiers.

*)

let stringLiteral =
   (let escapedQuote =
        %% "''" -|> "'" // A pair of single quotes escapes a single quote character
    let regularChars =
        many1Satisfy ((<>) '\'') // Any run of non-quote characters is literal
    %% '\'' -- +.([regularChars; escapedQuote] * qty.[0..]) -- '\''
    -|> String.Concat)
    <?> "string-literal"

let nameOrString =
    %[
        name
        stringLiteral
    ]

let nameOrKeyword =
    %[
        quotedName
        bracketedName
        backtickedName
        unquotedNameOrKeyword
        stringLiteral
    ]

(**

Since this is SQL we're dealing with, there are a bunch of special kinds of names.

Table names can have an optional schema qualifier part, like `main.Users`.
To avoid needless backtracking, we treat the second part as optional instead of the first for parsing purposes.

*)

let objectName =
    (%% +.nameOrKeyword
    -- ws
    -- +.(zeroOrOne * (%% '.' -- ws -? +.nameOrKeyword -- ws -|> id))
    -|> fun name name2 ->
        match name2 with
        | None -> { SchemaName = None; ObjectName = name }
        | Some name2 -> { SchemaName = Some name; ObjectName = name2 })
    <?> "table-name"

(**

Column names are similar -- they can be qualified with a table name, including its own optional schema qualifier.
We could parse this as an optional table name followed by the column name. But this would require extra effort
to ensure we parse "foo.bar" as (no schema, table foo, column bar), not (schema foo, table bar, no column).

So instead, we just parse between 1 and 3 names separated by dots, then put together the whole structure
based on how many we got.

*)

let columnName =
    (qty.[1..3] / tws '.' * tws name
    |>> fun names ->
        match names.Count with
        | 1 -> { Table = None; ColumnName = names.[0] }
        | 2 -> { Table = Some { SchemaName = None; ObjectName = names.[0] }; ColumnName = names.[1] }
        | 3 -> { Table = Some { SchemaName = Some names.[0]; ObjectName = names.[1] }; ColumnName = names.[2] }
        | _ -> failwith "Unreachable")
    <?> "column-name"

let qualifiedColumnName =
   (%% +.nameOrString
    -- ws
    -? '.'
    -- +.(qty.[0..2] / tws '.' * tws nameOrString)
    -|> fun initial rest ->
        match rest.Count with
        | 0 -> { Table = None; ColumnName = initial }
        | 1 -> { Table = Some { SchemaName = None; ObjectName = initial }; ColumnName = rest.[0] }
        | 2 -> { Table = Some { SchemaName = Some initial; ObjectName = rest.[0] }; ColumnName = rest.[1] }
        | _ -> failwith "Unreachable")
    <?> "qualified-column-name"

(**

Bind parameters in prepared statements can take [several forms](https://www.sqlite.org/lang_expr.html#varparam).

We'll support all of them except the special Tcl syntax for `$` parameters.

Named parameters are prefixed by `@`, `:`, or `$`.

*)

let namedBindParameter =
    %% +.['@'; ':'; '$']
    -- +.unquotedNameOrKeyword
    -|> fun prefix name -> NamedParameter (prefix, name)

(**

Positional parameters start with `?`, and may or may not contain an explicit integer parameter index.

*)

let positionalBindParameter =
    %% '?'
    -- +.(p<uint32> * zeroOrOne)
    -|> PositionalParameter

(*

Either parameter type is allowed anywhere bind parameters can be used.

*)

let bindParameter =
    %[ namedBindParameter; positionalBindParameter ]
    <?> "bind-parameter"

(**

Next up, a fundamental building block of expressions: literal values like `1` and `'str'`.

Some literals in SQL, such as `NULL`, are language keywords.

When we look for a keyword, we need to ensure that we don't actually read part of another name.
For example, if we're looking for CAST, we shouldn't match after reading the first four characters of
CASTLE. Therefore, we require that keywords are not followed by other legal identifier characters.

*)

let kw str =
    %% ci str
    -? notFollowedByL (satisfy isFollowingIdentifierCharacter) str
    -- ws
    -|> ()

(**

Keyword literals are very simple to define.

*)

let nullLiteral =
    %% kw "NULL" -|> NullLiteral

let currentTimeLiteral =
    %% kw "CURRENT_TIME" -|> CurrentTimeLiteral

let currentDateLiteral =
    %% kw "CURRENT_DATE" -|> CurrentDateLiteral

let currentTimestampLiteral =
    %% kw "CURRENT_TIMESTAMP" -|> CurrentTimestampLiteral

(**

Blob literals are described [here](https://www.sqlite.org/lang_expr.html):

> BLOB literals are string literals containing hexadecimal data and preceded
> by a single "x" or "X" character. Example: X'53514C697465'

We will be strict about this definition, requiring that only hexadecimal data
appear within the quotes of the literal, and that it contains an even number of
characters since there should be two hex digits for each octet.

Notice that we can still backtrack until parsing the first single quote.
Otherwise we would get confused whenever we see a column name starting with "x".

*)

let blobLiteral =
    let octet =
        %% +.(2, hex)
        -|> fun pair -> Byte.Parse(String(pair), NumberStyles.HexNumber)
    (%% ['x';'X']
    -? '\''
    -- +.(octet * qty.[0..])
    -- '\''
    -|> (Seq.toArray >> BlobLiteral))
    <?> "blob-literal"

(**

We could liberally parse integer and float literals using `pfloat` and `pint64`.

However, to strictly match the rules laid out
[here](https://www.sqlite.org/syntaxdiagrams.html#literal-value),
we use a custom FParsec `NumberLiteralOptions`.

*)

let numericLiteral =
    let options =
        NumberLiteralOptions.AllowHexadecimal
        ||| NumberLiteralOptions.AllowFraction
        ||| NumberLiteralOptions.AllowFractionWOIntegerPart
        ||| NumberLiteralOptions.AllowExponent
    numberLiteral options "numeric-literal" >>= fun lit ->
        if lit.IsInteger then
            lit.String |> uint64 |> IntegerLiteral |> preturn
        else if lit.IsHexadecimal then
            fail "hexadecimal floats are not permitted"
        else 
            lit.String |> float |> FloatLiteral |> preturn

(**

Our core numeric literal parser intentionally leaves out leading signs (`-` and `+`).
Most of the time, these are handled by the numeric literals appearing within expressions, in which
those are valid prefix operators.

However, sometimes numeric literals are accepted where general expressions are not. For these
situations, we have a special parser for a numeric literal prefixed by an optional sign.

*)

let signedNumericLiteral =
    let sign =
        %[
            %% '+' -|> 1
            %% '-' -|> -1
            preturn 0
        ]
    %% +.sign
    -- ws
    -- +.numericLiteral
    -|> fun sign value -> { Sign = sign; Value = value }

(**

In expressions, any kind of literal is OK.

*)

let literal =
    %[
        %% +.numericLiteral -|> NumericLiteral
        %% +.stringLiteral -|> StringLiteral
        blobLiteral
        nullLiteral
        currentTimeLiteral
        currentDateLiteral
        currentTimestampLiteral
    ]

(**

Next we're going to parse expressions -- the kind of expressions that can appear in a "where" clause, for example.
However, SQL expressions can be pretty complicated, so we have a lot of miscellaneous pieces to define
before we can get to something recognizable.

One of these pieces is the definition of a [type name](https://www.sqlite.org/syntax/type-name.html).
These are most frequently seen in `CREATE TABLE` statements, but also appear in expressions of the form
`CAST (expr as type-name)`.

Since SQLite cares little about types and cares very much about compatibility, it is very liberal about type names.
They can even have numeric bounds to look like types from other databases, even though SQLite ignores these completely.

*)

let typeBounds =
    %% '('
    -- ws
    -- +.(qty.[1..2] / tws ',' * tws signedNumericLiteral)
    -- ')'
    -- ws
    -|> fun bounds ->
        match bounds.Count with
        | 1 -> { Low = bounds.[0]; High = None }
        | 2 -> { Low = bounds.[0]; High = Some bounds.[1] }
        | _ -> failwith "Unreachable"

(**

The name part of the type name can consist of multiple identifiers, for compatibility with standard SQL nonsense
like `NATIONAL CHARACTER VARYING`.

*)

let typeName =
    (%% +.(qty.[1..] /. ws * name)
    -- +.(typeBounds * zeroOrOne)
    -|> fun name bounds -> { TypeName = name |> List.ofSeq; Bounds = bounds })
    <?> "type-name"

(**

The definition of the cast expression is relatively simple, but it relies on parsing an expression of
any type. Since we don't have the general expression parser yet, let's just take it as a parameter.

*)

let cast expr =
    %% kw "CAST"
    -- '('
    -- ws
    -- +.expr
    -- kw "AS"
    -- +. typeName
    -- ws
    -- ')'
    -|> fun ex typeName -> { Expression = ex; AsType = typeName }

(**

Function calls are another component of expressions. This includes aggregate functions,
so we have to support weird stuff like `count(*)` or `count(distinct id, name)`.

Once again, the expression parser is required to create this parser, so it's a parameter.

*)

let functionArguments (expr : Parser<Expr, unit>) =
    %[
        %% '*' -- ws -|> ArgumentWildcard
        %% +.((%% kw "DISTINCT" -- ws -|> Distinct) * zeroOrOne)
        -- +.(qty.[0..] / tws ',' * expr)
        -|> fun distinct args -> ArgumentList (distinct, args)
    ]

let functionInvocation expr =
    %% +.nameOrKeyword
    -- ws
    -? '('
    -- ws
    -- +.functionArguments expr
    -- ')'
    -|> fun name args -> { FunctionName = name; Arguments = args }

let case expr =
    let whenClause =
        %% kw "WHEN"
        -- +.expr
        -- kw "THEN"
        -- +.expr
        -%> auto
    let elseClause =
        %% kw "ELSE"
        -- +.expr
        -|> id
    let whenForm =
        %% +.(whenClause * qty.[1..])
        -- +.(elseClause * zeroOrOne)
        -- kw "END"
        -|> fun cases els -> { Input = None; Cases = cases; Else = els }
    let ofForm =
        %% +.expr
        -- +.whenForm
        -|> fun ofExpr case -> { case with Input = Some ofExpr }
    %% kw "CASE"
    -- +.[ whenForm; ofForm ]
    -|> id

let private binary op e1 e2 = BinaryExpr (op, e1, e2)
let private unary op e1 = UnaryExpr (op, e1)

let expr, private exprImpl = createParserForwardedToRef<Expr, unit>()
let selectStmt, private selectStmtImpl = createParserForwardedToRef<SelectStmt, unit>()

let tableInvocation =
    let args =
        %% '(' -- ws -- +.(qty.[0..] / tws ',' * expr) -- ')' -|> id
    %% +.objectName
    -- ws
    -- +.(args * zeroOrOne)
    -|> fun name args -> { Table = name; Arguments = args }

let collateOperator =
    %% kw "COLLATE"
    -- +.name
    -|> fun collation expr -> CollateExpr (expr, collation)

let isOperator =
    %% kw "IS"
    -- +.(zeroOrOne * kw "NOT")
    -|> function
    | Some () -> binary IsNot
    | None -> binary Is

let inOperator =
    %% +.(zeroOrOne * kw "NOT")
    -? kw "IN"
    -- +.[
            %% '('
            -- ws
            --
                +.[
                    %% +.selectStmt -|> InSelect
                    %% +.(qty.[0..] / tws ',' * expr) -|> InExpressions
                ]
            -- ')'
            -|> id
            %% +.tableInvocation -|> InTable
        ]
    -|> function
    | Some () -> fun inSet left -> NotInExpr (left, inSet)
    | None -> fun inSet left -> InExpr (left, inSet)

let similarityOperator =
    %% +.(zeroOrOne * kw "NOT")
    -? +.[
            %% kw "LIKE" -|> Like
            %% kw "GLOB" -|> Glob
            %% kw "MATCH" -|> Match
            %% kw "REGEXP" -|> Regexp
        ]
    -|> function
    | Some () -> fun op left right escape -> UnaryExpr (Not, SimilarityExpr (op, left, right, escape))
    | None -> fun op left right escape -> SimilarityExpr (op, left, right, escape)

let notNullOperator =
    %% [
        kw "NOTNULL"
        %% kw "NOT" -? kw "NULL" -|> ()
    ]
    -|> fun left -> BinaryExpr(IsNot, left, LiteralExpr NullLiteral)

let betweenOperator =
    %% +.(zeroOrOne * kw "NOT")
    -? kw "BETWEEN"
    -|> function
    | Some () -> fun input low high -> NotBetweenExpr (input, low, high)
    | None -> fun input low high -> BetweenExpr (input, low, high)

let raiseTrigger =
    %% kw "RAISE"
    -- '('
    -- ws
    -- +.[
            %% kw "IGNORE" -|> RaiseIgnore
            %% kw "ROLLBACK" -- ',' -- ws -- +.stringLiteral -- ws -|> RaiseRollback
            %% kw "ABORT" -- ',' -- ws -- +.stringLiteral -- ws -|> RaiseAbort
            %% kw "FAIL" -- ',' -- ws -- +.stringLiteral -- ws -|> RaiseFail
        ]
    -- ')'
    -|> RaiseExpr

let term expr =
    let parenthesized =
        %[
            %% +.selectStmt -|> ScalarSubqueryExpr
            expr
        ]
    %[
        %% '(' -- ws -- +.parenthesized -- ')' -|> id
        %% kw "EXISTS" -- ws -- '(' -- ws -- +.selectStmt -- ')' -|> ExistsExpr
        %% +.qualifiedColumnName -|> ColumnNameExpr
        %% +.literal -|> LiteralExpr
        %% +.bindParameter -|> BindParameterExpr
        %% +.cast expr -|> CastExpr
        %% +.case expr -|> CaseExpr
        raiseTrigger
        %% +.functionInvocation expr -|> FunctionInvocationExpr
        %% +.columnName -|> ColumnNameExpr
    ]

let private operators = [
    [
        postfixc collateOperator
    ]
    [
        prefix (kw "NOT") <| unary Not
        prefix '~' <| unary BitNot
        prefix '-' <| unary Negative
        prefix '+' id
    ]
    [
        infixl "||" <| binary Concatenate
    ]
    [
        infixl '*' <| binary Multiply
        infixl '/' <| binary Divide
        infixl '%' <| binary Modulo
    ]
    [
        infixl '+' <| binary Add
        infixl '-' <| binary Subtract
    ]
    [
        infixl "<<" <| binary BitShiftLeft
        infixl ">>" <| binary BitShiftRight
        infixl '&' <| binary BitAnd
        infixl '|' <| binary BitOr
    ]
    [
        infixl ">=" <| binary GreaterThanOrEqual
        infixl "<=" <| binary LessThanOrEqual
        infixl (%% '<' -? notFollowedBy (skipChar '>') -|> ()) <| binary LessThan
        infixl '>' <| binary GreaterThan
    ]
    [
        infixl "==" <| binary Equal
        infixl "=" <| binary Equal
        infixl "!=" <| binary NotEqual
        infixl "<>" <| binary NotEqual
        infixlc isOperator
        ternaryolc similarityOperator (kw "ESCAPE")
        postfix (kw "ISNULL") <| fun left -> BinaryExpr (Is, left, LiteralExpr NullLiteral)
        postfixc notNullOperator
        postfixc inOperator
        ternarylc betweenOperator (kw "AND")
    ]
    [
        infixl (kw "AND") <| binary And
    ]
    [
        infixl (kw "OR") <| binary Or
    ]
]

do
    exprImpl :=
        {
            Whitespace = ws
            Term = term
            Operators = operators    
        } |> Precedence.expression

let parenthesizedColumnNames =
    %% '('
    -- ws
    -- +.(qty.[0..] / tws ',' * tws nameOrString)
    -- ')'
    -- ws
    -|> id

let commonTableExpression =
    %% +.objectName
    -- ws
    -- +.(zeroOrOne * parenthesizedColumnNames)
    -- kw "AS"
    -- '('
    -- +.selectStmt
    -- ')'
    -- ws
    -|> fun table cols asSelect ->
        { Name = table; ColumnNames = cols; AsSelect = asSelect }

let withClause =
    %% kw "WITH"
    -- +.(zeroOrOne * tws (kw "RECURSIVE"))
    -- +.(qty.[1..] / tws ',' * commonTableExpression)
    -|> fun recurs ctes ->
        { Recursive = Option.isSome recurs; Tables = ctes }

let asAlias =
    %% (zeroOrOne * kw "AS")
    -? +.[ name; stringLiteral ]
    -|> id

let resultColumn =
    %% +.[
        %% '*' -|> ColumnsWildcard
        %% +.objectName -- '.' -? '*' -|> TableColumnsWildcard
        %% +.expr -- +.(asAlias * zeroOrOne) -|> fun ex alias -> Column (ex, alias)
    ] -- ws -|> id

let selectColumns =
    %% kw "SELECT"
    -- +.[
            %% kw "DISTINCT" -|> Some DistinctColumns
            %% kw "ALL" -|> Some AllColumns
            preturn None
        ]
    -- +.(qty.[1..] / tws ',' * resultColumn)
    -|> fun distinct cols -> { Distinct = distinct; Columns = cols }

let indexHint =
    %[
        %% kw "INDEXED" -- kw "BY" -- +.nameOrKeyword -- ws -|> IndexedBy
        %% kw "NOT" -- kw "INDEXED" -|> NotIndexed
    ]

let tableOrSubquery (tableExpr : Parser<TableExpr, unit>) =
    let subterm =
        %[
            %% +.selectStmt -|> fun select alias -> TableOrSubquery (Subquery (select, alias))
            %% +.tableExpr -|> fun table alias -> AliasedTableExpr (table, alias)
        ]
    let by =
        %[
            %% +.indexHint -|> fun indexed table -> TableOrSubquery (Table (table, None, Some indexed))
            %% +.(asAlias * zeroOrOne) -- +.(indexHint * zeroOrOne)
                -|> fun alias indexed table -> TableOrSubquery (Table (table, alias, indexed))
        ]

    %[
        %% +.tableInvocation -- +.by -|> fun table by -> by table
        %% '(' -- ws -- +.subterm -- ')' -- ws -- +.(asAlias * zeroOrOne) -|> (<|)
    ]

let joinType =
    %[
        %% kw "LEFT" -- (tws (kw "OUTER") * zeroOrOne) -|> LeftOuter
        %% kw "INNER" -|> Inner
        %% kw "CROSS" -|> Cross
        %% ws -|> Inner
    ]

let joinConstraint =
    %[
        %% kw "ON" -- +.expr -- ws -|> JoinOn
        %% kw "USING" -- '(' -- ws -- +.(qty.[1..] / tws ',' * columnName) -- ')' -- ws
            -|> fun cols -> JoinUsing (List.ofSeq cols)
        preturn JoinUnconstrained
    ]

let tableExpr =
    precursive <| fun tableExpr ->
        let term = tableOrSubquery tableExpr 
        let natural = %% kw "NATURAL" -|> ()   
        let join =
            %% +.[
                    %% ',' -|> fun left right constr -> Join (Inner, left, right, constr)
                    %% +.(natural * zeroOrOne) -- +.joinType -- kw "JOIN"
                        -|> fun natural join left right constr ->
                            let joinType = if Option.isSome natural then Natural join else join
                            Join (joinType, left, right, constr)
                ]
            -- ws
            -- +.term
            -- ws
            -- +.joinConstraint
            -|> fun f joinTo joinOn left -> f left joinTo joinOn
        %% +.term
        -- ws
        -- +.(join * qty.[0..])
        -|> Seq.fold (|>)

let valuesClause =
    let valuesRow =
        %% '('
        -- ws
        -- +.(qty.[0..] / tws ',' * expr)
        -- ')'
        -- ws
        -|> id

    %% kw "VALUES"
    -- ws
    -- +.(qty.[1..] / tws ',' * valuesRow)
    -- ws
    -|> id

let fromClause =
    %% kw "FROM"
    -- +.tableExpr
    -|> id

let whereClause =
    %% kw "WHERE"
    -- +.expr
    -|> id

let havingClause =
    %% kw "HAVING"
    -- +.expr
    -|> id

let groupByClause =
    %% kw "GROUP"
    -- kw "BY"
    -- +.(qty.[1..] / tws ',' * expr)
    -- +.(zeroOrOne * havingClause)
    -|> fun by having -> { By = by; Having = having }

let selectCore =
    %% +.selectColumns
    -- +.(fromClause * zeroOrOne)
    -- +.(whereClause * zeroOrOne)
    -- +.(groupByClause * zeroOrOne)
    -|> fun cols table where groupBy ->
        {
            Columns = cols
            From = table
            Where = where
            GroupBy = groupBy
        }

let compoundTerm =
    %[
        %% +.valuesClause -|> Values
        %% +.selectCore -|> Select
    ]

let compoundExpr =
    let compoundOperation =
        %[
            %% kw "UNION" -- +.(zeroOrOne * kw "ALL") -|> function
                | Some () -> fun left right -> UnionAll (left, right)
                | None -> fun left right -> Union (left, right)
            %% kw "INTERSECT" -|> fun left right -> Intersect (left, right)
            %% kw "EXCEPT" -|> fun left right -> Except (left, right)
        ]
    let compoundNext =
        %% +.compoundOperation
        -- +.compoundTerm
        -|> fun f right left -> f left right
    %% +.(compoundTerm |>> CompoundTerm)
    -- +.(compoundNext * qty.[0..])
    -|> Seq.fold (|>)

let orderDirection =
    %[
        %% kw "DESC" -|> Descending
        %% kw "ASC" -|> Ascending
        preturn Ascending
    ]

let orderingTerm =
    %% +.expr
    -- +.orderDirection
    -- ws
    -|> fun expr dir -> { By = expr; Direction = dir }

let orderBy =
    %% kw "ORDER"
    -- kw "BY"
    -- +.(qty.[1..] / tws ',' * orderingTerm)
    -|> id

let limit =
    let offset =
        %% [%% ',' -- ws -|> (); kw "OFFSET"]
        -- +.expr
        -|> id
    %% kw "LIMIT"
    -- +.expr
    -- +.(zeroOrOne * offset)
    -|> fun limit offset -> { Limit = limit; Offset = offset }

do
    selectStmtImpl :=
        (
            %% +.(zeroOrOne * withClause)
            -? +.compoundExpr
            -- +.(zeroOrOne * orderBy)
            -- +.(zeroOrOne * limit)
            -|> fun cte comp orderBy limit ->
                {
                    With = cte
                    Compound = comp
                    OrderBy = orderBy
                    Limit = limit
                }
        )

let conflictClause =
    let onConflict =
        %% kw "ON" -- kw "CONFLICT" -|> ()
    let clause =
        %% (onConflict * zeroOrOne)
        -- +.[
                %% kw "ROLLBACK" -|> Rollback
                %% kw "ABORT" -|> Abort
                %% kw "FAIL" -|> Fail
                %% kw "IGNORE" -|> Ignore
                %% kw "REPLACE" -|> Replace
            ]
        -|> id
    zeroOrOne * clause

let foreignKeyRule =
    let eventRule =
        %% kw "ON"
        -- +.[
                %% kw "DELETE" -|> OnDelete
                %% kw "UPDATE" -|> OnUpdate
            ]
        -- +.[
                %% kw "SET" -- +.[ %% kw "NULL" -|> SetNull; %% kw "DEFAULT" -|> SetDefault ] -|> id
                %% kw "CASCADE" -|> Cascade
                %% kw "RESTRICT" -|> Restrict
                %% kw "NO" -- kw "ACTION" -|> NoAction
            ]
        -|> fun evt handler -> EventRule (evt, handler)
    let matchRule =
        %% kw "MATCH"
        -- +.name
        -- ws
        -|> MatchRule
    %[ eventRule; matchRule ]


let foreignKeyDeferClause =
    let initially =
        %% kw "INITIALLY" -- +.[ %% kw "DEFERRED" -|> true; %% kw "IMMEDIATE" -|> false ] -|> id
    %% +.(zeroOrOne * kw "NOT")
    -? kw "DEFERRABLE"
    -- +.(zeroOrOne * initially)
    -|> fun not init -> { Deferrable = Option.isNone not; InitiallyDeferred = init }

let foreignKeyClause =
    %% kw "REFERENCES"
    -- +.objectName
    -- +.(zeroOrOne * parenthesizedColumnNames)
    -- +.(qty.[0..] * foreignKeyRule)
    -- +.(zeroOrOne * foreignKeyDeferClause)
    -|> fun table cols rules defer ->
        {
            ReferencesTable = table
            ReferencesColumns = cols
            Rules = rules
            Defer = defer
        }

let constraintName =
    %% kw "CONSTRAINT"
    -- +.name
    -- ws
    -|> id

let primaryKeyClause =
    %% kw "PRIMARY"
    -- kw "KEY"
    -- +.orderDirection
    -- ws
    -- +.conflictClause
    -- +.(zeroOrOne * tws (kw "AUTOINCREMENT"))
    -|> fun dir conflict auto ->
        {
            Order = dir
            ConflictClause = conflict
            AutoIncrement = Option.isSome auto
        }

let constraintType =
    let signedToExpr (signed : SignedNumericLiteral) =
        let expr = signed.Value |> NumericLiteral |> LiteralExpr
        if signed.Sign < 0 then UnaryExpr (Negative, expr)
        else expr
    let defaultValue =
        %[
            %% +.signedNumericLiteral -|> signedToExpr
            %% +.literal -|> LiteralExpr
            %% '(' -- ws -- +.expr -- ')' -|> id
            // docs don't mention this, but it works
            %% +.name -|> (StringLiteral >> LiteralExpr)
        ]
    %[
        %% +.primaryKeyClause -|> PrimaryKeyConstraint
        %% kw "NOT"  -- kw "NULL" -- +.conflictClause -|> NotNullConstraint
        %% kw "NULL" -|> NullableConstraint
        %% kw "UNIQUE" -- +.conflictClause -|> UniqueConstraint
        %% kw "CHECK" -- '(' -- ws -- +.expr -- ')' -|> CheckConstraint
        %% kw "DEFAULT" -- +.defaultValue -|> DefaultConstraint
        %% kw "COLLATE" -- +.name -|> CollateConstraint
        %% +.foreignKeyClause -|> ForeignKeyConstraint
    ]

let columnConstraint =
    %% +.(zeroOrOne * constraintName)
    -- +.constraintType
    -- ws
    -|> fun name cty -> { Name = name; ColumnConstraintType = cty }

let columnDef =
    %% +.nameOrKeyword
    -- ws
    -- +.(typeName * zeroOrOne)
    -- +.(columnConstraint * qty.[0..])
    -|> fun name typeName constraints ->
        {
            Name = name
            Type = typeName
            Constraints = constraints
        }

let alterTableStmt =
    let renameTo =  
        %% kw "RENAME"
        -- kw "TO"
        -- +.name
        -|> RenameTo
    let addColumn =
        %% kw "ADD"
        -- zeroOrOne * kw "COLUMN"
        -- +.columnDef
        -|> AddColumn
    %% kw "ALTER"
    -- kw "TABLE"
    -- +.objectName
    -- +.[ renameTo; addColumn ]
    -|> fun table alteration -> { Table = table; Alteration = alteration }

let tableIndexConstraintType =
    %[
        %% kw "PRIMARY" -- kw "KEY" -|> PrimaryKey
        %% kw "UNIQUE" -|> Unique
    ]

let indexedColumns =
    %% '('
    -- ws
    -- +.(qty.[1..] / tws ',' * (%% +.expr -- +.orderDirection -%> auto))
    -- ')'
    -- ws
    -|> id

let tableIndexConstraint =
    %% +.tableIndexConstraintType
    -- +.indexedColumns
    -- +.conflictClause
    -|> fun cty cols conflict ->
        { Type = cty; IndexedColumns = cols; ConflictClause = conflict }

let tableConstraintType =
    let foreignKey =
        %% kw "FOREIGN"
        -- kw "KEY"
        -- +.parenthesizedColumnNames
        -- +.foreignKeyClause
        -|> fun columns fk -> TableForeignKeyConstraint (columns, fk)
    %[
        %% kw "CHECK" -- '(' -- ws -- +.expr -- ')' -|> TableCheckConstraint
        foreignKey
        %% +.tableIndexConstraint -|> TableIndexConstraint
    ]

let tableConstraint =
    %% +.(zeroOrOne * constraintName)
    -- +.tableConstraintType
    -- ws
    -|> fun name cty -> { Name = name; TableConstraintType = cty }

let createTableDefinition =
    let part =
        %[
            %% +.tableConstraint -|> Choice1Of2
            %% +.columnDef -|> Choice2Of2
        ]
    %% '('
    -- ws
    -- +.(qty.[0..] /. tws ',' * part)
    -- ')'
    -- ws
    -- +.(zeroOrOne * (%% kw "WITHOUT" -- kw "ROWID" -- ws -|> ()))
    -|> fun parts without ->
        {
            Columns =
                parts |> Seq.choose (function | Choice2Of2 cdef -> Some cdef | Choice1Of2 _ -> None) |> ResizeArray
            Constraints =
                parts |> Seq.choose (function | Choice1Of2 ct -> Some ct | Choice2Of2 _ -> None) |> ResizeArray
            WithoutRowId = Option.isSome without
        }

let createTableAs =
    %[
        %% kw "AS" -- +.selectStmt -|> CreateAsSelect
        %% +.createTableDefinition -|> CreateAsDefinition
    ]

let ifNotExists = %(zeroOrOne * (%% kw "IF" -- kw "NOT" -- kw "EXISTS" -|> ()))

let temporary = %(zeroOrOne * [kw "TEMPORARY"; kw "TEMP"])
        
let createTableStmt =
    %% kw "CREATE"
    -- +.temporary
    -? kw "TABLE"
    -- +.ifNotExists
    -- +.objectName
    -- +.createTableAs
    -|> fun temp ifNotExists name createAs ->
        {
            Temporary = Option.isSome temp
            IfNotExists = Option.isSome ifNotExists
            Name = name
            As = createAs
        }

let analyzeStmt =
    %% kw "ANALYZE"
    -- +.(zeroOrOne * objectName)
    -|> id

let attachStmt =
    %% kw "ATTACH"
    -- zeroOrOne * kw "DATABASE"
    -- +.expr
    -- kw "AS"
    -- +.nameOrKeyword
    -|> fun ex schemaName -> ex, schemaName

let transactionType =
    %[
        %% kw "DEFERRED" -|> Deferred
        %% kw "IMMEDIATE" -|> Immediate
        %% kw "EXCLUSIVE" -|> Exclusive
        preturn Deferred
    ]

let beginStmt =
    %% kw "BEGIN"
    -- +.transactionType
    -- zeroOrOne * kw "TRANSACTION"
    -- zeroOrOne * nameOrString // optional ignored name
    -|> BeginStmt

let commitStmt =
    %% [ kw "COMMIT"; kw "END" ]
    -- zeroOrOne * kw "TRANSACTION"
    -|> CommitStmt

let rollbackStmt =
    let toPoint =
        %% kw "TO"
        -- zeroOrOne * kw "SAVEPOINT"
        -- +.name
        -|> RollbackToSavepoint
    let tx =
        %% +.nameOrString -|> RollbackTransactionByName
    %% kw "ROLLBACK"
    -- zeroOrOne * kw "TRANSACTION"
    -- +.[
            toPoint
            tx
            preturn RollbackTransaction
        ]
    -|> RollbackStmt

let createIndexStmt =
    %% kw "CREATE"
    -- +.(zeroOrOne * kw "UNIQUE")
    -? kw "INDEX"
    -- +.ifNotExists
    -- +.objectName
    -- kw "ON"
    -- +.objectName
    -- +.indexedColumns
    -- +.(zeroOrOne * (%% kw "WHERE" -- +.expr -|> id))
    -|> fun unique ifNotExists indexName tableName cols whereExpr ->
        {
            Unique = Option.isSome unique
            IfNotExists = Option.isSome ifNotExists
            IndexName = indexName
            TableName = tableName
            IndexedColumns = cols
            Where = whereExpr
        }

let qualifiedTableName =
    %% +.objectName
    -- +.(zeroOrOne * indexHint)
    -|> fun tableName hint ->
        {
            TableName = tableName
            IndexHint = hint
        }

let deleteStmt =
    %% +.(zeroOrOne * withClause)
    -? kw "DELETE"
    -- kw "FROM"
    -- +.qualifiedTableName
    -- +.(zeroOrOne * whereClause)
    -- +.(zeroOrOne * orderBy)
    -- +.(zeroOrOne * limit)
    -|> fun withClause fromTable where orderBy limit ->
        {
            With = withClause
            DeleteFrom = fromTable
            Where = where
            OrderBy = orderBy
            Limit = limit
        }

let updateOr =
    %% kw "OR"
    -- +.[
            %% kw "ROLLBACK" -|> UpdateOrRollback
            %% kw "ABORT" -|> UpdateOrAbort
            %% kw "REPLACE" -|> UpdateOrReplace
            %% kw "FAIL" -|> UpdateOrFail
            %% kw "IGNORE" -|> UpdateOrIgnore
        ]
    -|> id

let updateStmt =
    let setColumn =
        %% +.nameOrString
        -- ws
        -- '='
        -- ws
        -- +.expr
        -|> fun name expr -> name, expr
    %% +.(zeroOrOne * withClause)
    -? kw "UPDATE"
    -- +.(zeroOrOne * updateOr)
    -- +.qualifiedTableName
    -- kw "SET"
    -- +.(qty.[1..] / tws ',' * setColumn)
    -- +.(zeroOrOne * whereClause)
    -- +.(zeroOrOne * orderBy)
    -- +.(zeroOrOne * limit)
    -|> fun withClause updateOr table sets where orderBy limit ->
        {
            With = withClause
            UpdateTable = table
            Or = updateOr
            Set = sets
            Where = where
            OrderBy = orderBy
            Limit = limit
        }

let insertOr =
    let orPart =
        %% kw "OR"
        -- +.[
                %% kw "REPLACE" -|> InsertOrReplace
                %% kw "ROLLBACK" -|> InsertOrRollback
                %% kw "ABORT" -|> InsertOrAbort
                %% kw "FAIL" -|> InsertOrFail
                %% kw "IGNORE" -|> InsertOrIgnore
            ]
        -|> id
    %[
        %% kw "REPLACE" -|> Some InsertOrReplace
        %% kw "INSERT" -- +.(zeroOrOne * orPart) -|> id
    ]

let insertStmt =
    %% +.(zeroOrOne * withClause)
    -? +.insertOr
    -- kw "INTO"
    -- +.objectName
    -- +.(zeroOrOne * parenthesizedColumnNames)
    -- +.[
            %% kw "DEFAULT" -- kw "VALUES" -|> None
            %% +.selectStmt -|> Some
        ]
    -|> fun withClause insert table cols data ->
        {
            With = withClause
            Or = insert
            InsertInto = table
            Columns = cols
            Data = data
        }

let triggerSchedule =
    %[
        %% kw "BEFORE" -|> Before
        %% kw "AFTER" -|> After
        %% kw "INSTEAD" -- kw "OF" -|> InsteadOf
        preturn Before
    ]

let triggerCause =
    let updateColumns =
        %% kw "OF" -- +.(qty.[1..] / tws ',' * tws name) -|> id
    %[
        %% kw "DELETE" -|> DeleteOn
        %% kw "INSERT" -|> InsertOn
        %% kw "UPDATE" -- +.(zeroOrOne * updateColumns) -|> UpdateOn
    ]

let triggerAction =
    %[
        %% +.selectStmt -|> TriggerSelect
        %% +.deleteStmt -|> TriggerDelete
        %% +.updateStmt -|> TriggerUpdate
        %% +.insertStmt -|> TriggerInsert
    ]

let createTriggerStmt =
    let whenClause =
        %% kw "WHEN"
        -- +.expr
        -|> id
    %% kw "CREATE"
    -- +.temporary
    -? kw "TRIGGER"
    -- +.ifNotExists
    -- +.objectName
    -- +.triggerSchedule
    -- +.triggerCause
    -- kw "ON"
    -- +.objectName
    -- zeroOrOne * (%% kw "FOR" -- kw "EACH" -- kw "ROW" -|> ())
    -- +.(zeroOrOne * whenClause)
    -- kw "BEGIN"
    -- +.(qty.[1..] /. tws ';' * tws triggerAction)
    -- kw "END"
    -|> fun temp ifne triggerName schedule cause tableName whenClause actions ->
        {
            Temporary = Option.isSome temp
            IfNotExists = Option.isSome ifne
            TriggerName = triggerName
            TableName = tableName
            Schedule = schedule
            Cause = cause
            Condition = whenClause
            Actions = actions
        }

let createViewStmt =
    %% kw "CREATE"
    -- +.temporary
    -? kw "VIEW"
    -- +.ifNotExists
    -- +.objectName
    -- +.(zeroOrOne * parenthesizedColumnNames)
    -- kw "AS"
    -- +.selectStmt
    -|> fun temp ifNotExists viewName cols asSelect ->
        {
            Temporary = Option.isSome temp
            IfNotExists = Option.isSome ifNotExists
            ViewName = viewName
            ColumnNames = cols
            AsSelect = asSelect
        }

let createVirtualTableStmt =
    let moduleArgumentTopChunk =
        many1Satisfy (fun c -> c <> '(' && c <> ')' && c <> ',')
    let moduleArgumentInnerChunk =
        many1Satisfy (fun c -> c <> '(' && c <> ')')
    let moduleArgumentNest =
        precursive <| fun moduleArgumentNest ->
            let chunk = moduleArgumentNest <|> moduleArgumentInnerChunk
            %% '('
            -- +.manyStrings chunk
            -- ')'
            -|> fun s -> "(" + s + ")"
    let moduleArgument =
        let chunk = moduleArgumentNest <|> moduleArgumentTopChunk
        manyStrings chunk
    let moduleArguments =
        %% '('
        -- ws
        -- +.(qty.[0..] / ',' * moduleArgument)
        -- ')'
        -|> id
    %% kw "CREATE"
    -? kw "VIRTUAL"
    -- kw "TABLE"
    -- +.ifNotExists
    -- +.objectName
    -- kw "USING"
    -- +.name
    -- ws
    -- +.(zeroOrOne * moduleArguments)
    -|> fun ifNotExists vTableName usingModule withArgs ->
        {
            IfNotExists = Option.isSome ifNotExists
            VirtualTable = vTableName
            UsingModule = usingModule
            WithModuleArguments = defaultArg withArgs (new ResizeArray<_>())
        }

let detachStmt =
    %% kw "DETACH"
    -- zeroOrOne * kw "DATABASE"
    -- +.nameOrKeyword
    -|> DetachStmt

let ifExists =
    %[
        %% kw "IF" -- kw "EXISTS" -|> true
        preturn false
    ]

let dropObjectType =
    %[
        %% kw "INDEX" -|> DropIndex
        %% kw "TABLE" -|> DropTable
        %% kw "TRIGGER" -|> DropTrigger
        %% kw "VIEW" -|> DropView
    ]

let dropObjectStmt =
    %% kw "DROP"
    -? +.dropObjectType
    -- +.ifExists
    -- +.objectName
    -|> fun dropType ifExists name ->
        { Drop = dropType; IfExists = ifExists; IndexName = name }

let pragmaValue =
    let interiorValue =
        %[
            %% +.nameOrKeyword -|> StringPragmaValue
            %% +.signedNumericLiteral -|> NumericPragmaValue
        ]
    %[
        %% '(' -- ws -- +.interiorValue -- ws -- ')' -|> id
        %% '=' -- ws -- +.interiorValue -|> id
    ]

let pragmaStmt =
    %% kw "PRAGMA"
    -- +.objectName
    -- +.(zeroOrOne * pragmaValue)
    -|> fun name value ->
        {
            Pragma = name
            Value = value
        }

let reindexStmt =
    %% kw "REINDEX"
    -- +.(zeroOrOne * objectName)
    -|> ReindexStmt

let releaseStmt =
    %% kw "RELEASE"
    -- zeroOrOne * kw "SAVEPOINT"
    -- +.name
    -|> ReleaseStmt

let savepointStmt =
    %% kw "SAVEPOINT"
    -- +.name
    -|> SavepointStmt

let vacuumStmt =
    %% kw "VACUUM"
    -|> VacuumStmt

let private almostAnyStmt =
    %[
        %% +.alterTableStmt -|> AlterTableStmt
        %% +.analyzeStmt -|> AnalyzeStmt
        %% +.attachStmt -|> AttachStmt
        beginStmt
        commitStmt
        %% +.createIndexStmt -|> CreateIndexStmt
        %% +.createTableStmt -|> CreateTableStmt
        %% +.createTriggerStmt -|> CreateTriggerStmt
        %% +.createViewStmt -|> CreateViewStmt
        %% +.createVirtualTableStmt -|> CreateVirtualTableStmt
        %% +.deleteStmt -|> DeleteStmt
        detachStmt
        %% +.dropObjectStmt -|> DropObjectStmt
        %% +.insertStmt -|> InsertStmt
        %% +.pragmaStmt -|> PragmaStmt
        reindexStmt
        releaseStmt
        rollbackStmt
        savepointStmt
        %% +.selectStmt -|> SelectStmt
        %% +.updateStmt -|> UpdateStmt
        vacuumStmt
    ]

let explainStmt =
    %% kw "EXPLAIN"
    -- (zeroOrOne * (%% kw "QUERY" -- kw "PLAN" -|> ()))
    -- +.almostAnyStmt
    -|> ExplainStmt

let anyStmt = %[ explainStmt; almostAnyStmt ]

let private stmtsAtLeast min =
    %% ws
    -- +.(qty.[min..] /. tws ';' * tws anyStmt)
    -|> List.ofSeq

let stmts = stmtsAtLeast 0
let stmts1 = stmtsAtLeast 1