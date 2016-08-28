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
    %% "--" -- restOfLine true -%> ()

/// A block comment begins with /* and continues until a trailing */ is found.
/// Nested block comments are not allowed, so additional /* tokens found
/// after the first are ignored.
let blockComment =
    %% "/*" -- skipCharsTillString "*/" true Int32.MaxValue -%> ()

/// Where whitespace is expected, it can be one of...
let whitespaceUnit =
    %[
        lineComment // a line comment
        blockComment // a block comment
        spaces1 // one or more whitespace characters
    ]

/// Optional whitespace: 0 or more whitespace units
let ws = skipMany whitespaceUnit

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
        %% "\"\"" -%> "\"" // A pair of double quotes escapes a double quote character
    let regularChars =
        many1Satisfy ((<>) '"') // Any run of non-quote characters is literal
    %% '"' -- +.([regularChars; escapedQuote] * qty.[0..]) -- '"'
    -%> String.Concat // Glue together the parts of the string

/// A name wrapped in square brackets (T-SQL style).
let bracketedName =
    let escapedBracket =
        %% "]]" -%> "]" // A pair of right brackets escapes a right bracket character
    let regularChars =
        many1Satisfy ((<>) ']') // Any run of non-bracket characters is literal
    %% '[' -- +.([regularChars; escapedBracket] * qty.[0..]) -- ']'
    -%> String.Concat

/// A name wrapped in backticks (MySQL style)
let backtickedName =
    let escapedTick =
        %% "``" -%> "`" // A pair of backticks escapes a backtick character
    let regularChars =
        many1Satisfy ((<>) '`') // Any run of non-backtick characters is literal
    %% '`' -- +.([regularChars; escapedTick] * qty.[0..]) -- '`'
    -%> String.Concat

(**

For unquoted names, we should ensure that a reserved keyword is not used.

*)

let sqlKeywords =
    [
        "ABORT"; "ACTION"; "ADD"; "AFTER"; "ALL"; "ALTER"; "ANALYZE";
        "AND"; "AS"; "ASC"; "ATTACH"; "AUTOINCREMENT"; "BEFORE"; "BEGIN";
        "BETWEEN"; "BY"; "CASCADE"; "CASE"; "CAST"; "CHECK"; "COLLATE";
        "COLUMN"; "COMMIT"; "CONFLICT"; "CONSTRAINT"; "CREATE"; "CROSS";
        "CURRENT_DATE"; "CURRENT_TIME"; "CURRENT_TIMESTAMP"; "DATABASE";
        "DEFAULT"; "DEFERRABLE"; "DEFERRED"; "DELETE"; "DESC"; "DETACH";
        "DISTINCT"; "DROP"; "EACH"; "ELSE"; "END"; "ESCAPE"; "EXCEPT";
        "EXCLUSIVE"; "EXISTS"; "EXPLAIN"; "FAIL"; "FOR"; "FOREIGN"; "FROM";
        "FULL"; "GLOB"; "GROUP"; "HAVING"; "IF"; "IGNORE"; "IMMEDIATE"; "IN";
        "INDEX"; "INDEXED"; "INITIALLY"; "INNER"; "INSERT"; "INSTEAD";
        "INTERSECT"; "INTO"; "IS"; "ISNULL"; "JOIN"; "KEY"; "LEFT"; "LIKE";
        "LIMIT"; "MATCH"; "NATURAL"; "NO"; "NOT"; "NOTNULL"; "NULL"; "OF";
        "OFFSET"; "ON"; "OR"; "ORDER"; "OUTER"; "PLAN"; "PRAGMA"; "PRIMARY";
        "QUERY"; "RAISE"; "RECURSIVE"; "REFERENCES"; "REGEXP"; "REINDEX";
        "RELEASE"; "RENAME"; "REPLACE"; "RESTRICT"; "RIGHT"; "ROLLBACK"; "ROW";
        "SAVEPOINT"; "SELECT"; "SET"; "TABLE"; "TEMP"; "TEMPORARY"; "THEN";
        "TO"; "TRANSACTION"; "TRIGGER"; "UNION"; "UNIQUE"; "UPDATE"; "USING";
        "VACUUM"; "VALUES"; "VIEW"; "VIRTUAL"; "WHEN"; "WHERE"; "WITH"; "WITHOUT"
    ] |> fun kws ->
        HashSet<string>(kws, StringComparer.OrdinalIgnoreCase)
        // Since SQL is case-insensitive, be sure to ignore case
        // in this hash set.

let isInitialIdentifierCharacter c =
    c = '_'
    || c = '$'
    || c >= 'a' && c <= 'z'
    || c >= 'A' && c <= 'Z'
let isFollowingIdentifierCharacter c =
    isInitialIdentifierCharacter c || c >= '0' && c <= '9'

/// A plain, unquoted name
let unquotedName =
    let identifier =
        many1Satisfy2 isInitialIdentifierCharacter isFollowingIdentifierCharacter
    identifier >>= fun ident ->
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

When we look for a keyword, we need to ensure that we don't actually read part of another name.
For example, if we're looking for CAST, we shouldn't match after reading the first four characters of
CASTLE. Therefore, we require that keywords are not followed by other legal identifier characters.

*)

let kw str =
    %% ci str -? notFollowedByL (satisfy isFollowingIdentifierCharacter) str -%> ()

(**

The simplest literals are those with only one representation.

*)

let nullLiteral =
    %% kw "NULL" -%> NullLiteral

let currentTimeLiteral =
    %% kw "CURRENT_TIME" -%> CurrentTimeLiteral

let currentDateLiteral =
    %% kw "CURRENT_DATE" -%> CurrentDateLiteral

let currentTimestampLiteral =
    %% kw "CURRENT_TIMESTAMP" -%> CurrentTimestampLiteral

(**

Next are string literals, which are very similar to quoted identifiers.

*)

let stringLiteral =
    let escapedQuote =
        %% "''" -%> "'" // A pair of single quotes escapes a single quote character
    let regularChars =
        many1Satisfy ((<>) '\'') // Any run of non-quote characters is literal
    %% '\'' -- +.([regularChars; escapedQuote] * qty.[0..]) -- '\''
    -%> (String.Concat >> StringLiteral)

(**

Blob literals are described [here](https://www.sqlite.org/lang_expr.html):

> BLOB literals are string literals containing hexadecimal data and preceded
> by a single "x" or "X" character. Example: X'53514C697465'

We will be strict about this definition, requiring that only hexadecimal data
appear within the quotes of the literal, and that it contains an even number of
characters since there should be two hex digits for each octet.

*)

let blobLiteral =
    let octet =
        %% +.(2, hex)
        -%> fun pair -> Byte.Parse(String(pair), NumberStyles.HexNumber)
    %% ['x';'X'] -- '\''
    -- +.(octet * qty.[0..])
    -- '\''
    -%> (Seq.toArray >> BlobLiteral)

(**

We could liberally parse integer and float literals using pfloat and pint64.

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
    numberLiteral options "numeric literal" >>= fun lit ->
        if lit.IsInteger then
            lit.String |> int64 |> IntegerLiteral |> preturn
        else if lit.IsHexadecimal then
            fail "hexadecimal floats are not permitted"
        else 
            lit.String |> float |> FloatLiteral |> preturn

let signedNumericLiteral =
    let sign =
        %[
            %% '+' -%> 1
            %% '-' -%> -1
            preturn 0
        ]
    %% +.sign
    -- ws
    -- +.numericLiteral
    -%> fun sign value -> { Sign = sign; Value = value }

(**

We then combine the literal definitions into one.

*)

let literal =
    %[
        %% +.numericLiteral -%> NumericLiteral
        stringLiteral
        blobLiteral
        nullLiteral
        currentTimeLiteral
        currentDateLiteral
        currentTimestampLiteral
    ]

let typeBounds =
    %% '('
    -- ws
    -- +.(qty.[1..2] / ',' * (signedNumericLiteral .>> ws))
    -- ')'
    -%> fun bounds ->
        match bounds.Count with
        | 1 -> { Low = bounds.[0]; High = None }
        | 2 -> { Low = bounds.[0]; High = Some bounds.[1] }
        | _ -> failwith "Unreachable"

let typeName =
    %% +.(qty.[1..] /. ws * name)
    -- +.(typeBounds * zeroOrOne)
    -%> fun name bounds -> { TypeName = name |> List.ofSeq; Bounds = bounds }
    
let tableName =
    qty.[1..2] / '.' * (name .>> ws)
    |>> fun names ->
        match names.Count with
        | 1 -> { SchemaName = None; TableName = names.[0] }
        | 2 -> { SchemaName = Some names.[0]; TableName = names.[1] }
        | _ -> failwith "Unreachable"

let columnName =
    qty.[1..3] / '.' * (name .>> ws)
    |>> fun names ->
        match names.Count with
        | 1 -> { Table = None; ColumnName = names.[0] }
        | 2 -> { Table = Some { SchemaName = None; TableName = names.[0] }; ColumnName = names.[1] }
        | 3 -> { Table = Some { SchemaName = Some names.[0]; TableName = names.[1] }; ColumnName = names.[2] }
        | _ -> failwith "Unreachable"

let indexName = name // TODO: can this have a schema name too?

let namedBindParameter =
    %% +.['@'; ':'; '$']
    -- +.name
    -%> fun prefix name -> NamedParameter (prefix, name)

let positionalBindParameter =
    %% '?'
    -- +.(p<uint32> * zeroOrOne)
    -%> PositionalParameter

let bindParameter = %[ namedBindParameter; positionalBindParameter ]

let functionArguments (expr : Parser<Expr, unit>) =
    %[
        %% '*' -- ws -%> ArgumentWildcard
        %% +.((%% kw "DISTINCT" -- ws -%> Distinct) * zeroOrOne)
        -- +.(qty.[0..] / ',' * expr)
        -%> fun distinct args -> ArgumentList (distinct, args)
    ]

let functionInvocation expr =
    %% +.name
    -- ws
    -? '('
    -- ws
    -- +.functionArguments expr
    -- ')'
    -%> fun name args -> { FunctionName = name; Arguments = args }

let cast expr =
    %% kw "CAST"
    -- ws
    -- '('
    -- ws
    -- +.expr
    -- kw "AS"
    -- ws
    -- +. typeName
    -- ws
    -- ')'
    -%> fun ex typeName -> { Expression = ex; AsType = typeName }

let case expr =
    let whenClause =
        %% kw "WHEN"
        -- ws
        -- +.expr
        -- kw "THEN"
        -- ws
        -- +.expr
        -%> auto
    let elseClause =
        %% kw "ELSE"
        -- ws
        -- +.expr
        -%> id
    %% kw "CASE"
    -- ws
    -- +.(expr * zeroOrOne)
    -- +.(whenClause * qty.[1..])
    -- +.(elseClause * zeroOrOne)
    -- kw "END"
    -%> fun input cases els ->
        { Input = input; Cases = cases; Else = els }

let private binary op e1 e2 = BinaryExpr (op, e1, e2)
let private unary op e1 = UnaryExpr (op, e1)

let private expr, exprImpl = createParserForwardedToRef<Expr, unit>()
let private selectStmt, selectStmtImpl = createParserForwardedToRef<SelectStmt, unit>()

let tableInvocation =
    let args =
        %% '(' -- ws -- +.(qty.[0..] / ',' * expr) -- ')' -%> auto
    %% +.tableName
    -- ws
    -- +.(args * zeroOrOne)
    -%> fun name args -> { Table = name; Arguments = args }

let collateOperator =
    %% kw "COLLATE"
    -- ws1
    -- +.name
    -%> fun collation expr -> CollateExpr (expr, collation)

let isOperator =
    %% kw "IS"
    -- +.((%% ws1 -- kw "NOT" -%> ()) * zeroOrOne)
    -%> function
    | Some () -> binary IsNot
    | None -> binary Is

let inOperator =
    %% +.((%% kw "NOT" -- ws1 -%> ()) * zeroOrOne)
    -- kw "IN"
    ?- ws
    -- +.[
            %% '('
            -- ws
            --
                +.[
                    %% +.selectStmt -%> InSelect
                    %% +.(qty.[0..] / ',' * expr) -%> InExpressions
                ]
            -- ')'
            -%> id
            %% +.tableInvocation -%> InTable
        ]
    -%> function
    | Some () -> fun inSet left -> NotInExpr (left, inSet)
    | None -> fun inSet left -> InExpr (left, inSet)

let similarityOperator =
    %% +.((%% kw "NOT" -- ws1 -%> ()) * zeroOrOne)
    -? +.[
            %% kw "LIKE" -%> Like
            %% kw "GLOB" -%> Glob
            %% kw "MATCH" -%> Match
            %% kw "REGEXP" -%> Regexp
        ]
    -%> function
    | Some () -> fun op left right escape -> UnaryExpr (Not, SimilarityExpr (op, left, right, escape))
    | None -> fun op left right escape -> SimilarityExpr (op, left, right, escape)

let notNullOperator =
    %% kw "NOT"
    -- ws // compound "NOTNULL" is allowed too, so this is optional whitespace
    -? kw "NULL"
    -%> fun left -> BinaryExpr(IsNot, left, LiteralExpr NullLiteral)

let betweenOperator =
    %% +.((%% kw "NOT" -- ws1 -%> ()) * zeroOrOne)
    -? kw "BETWEEN"
    -%> function
    | Some () -> fun input low high -> NotBetweenExpr (input, low, high)
    | None -> fun input low high -> BetweenExpr (input, low, high)

let existsOperator =
    %% +.((%% kw "NOT" -- ws1 -%> ()) * zeroOrOne)
    -? kw "EXISTS"
    -- ws
    -- '('
    -- ws
    -- +.selectStmt
    -- ')'
    -%> function
    | Some () -> fun select input -> NotExistsExpr (input, select)
    | None -> fun select input -> ExistsExpr (input, select)

let term expr =
    %[
        %% '(' -- ws -- +. expr -- ')' -%> auto
        %% +.literal -%> LiteralExpr
        %% +.bindParameter -%> BindParameterExpr
        %% +.columnName -%> ColumnNameExpr
        %% +.cast expr -%> CastExpr
        %% +.case expr -%> CaseExpr
        %% +.functionInvocation expr -%> FunctionInvocationExpr
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
        infixl (%'<' >>. notFollowedBy (skipChar '>')) <| binary LessThan
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
        postfixc existsOperator
        ternarylc betweenOperator (kw "AND")
    ]
    [
        infixl (kw "AND") <| binary And
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

let commonTableExpression =
    let columnNames =
        %% '('
        -- ws
        -- +.(qty.[0..] / ',' * (columnName .>> ws))
        -- ')'
        -%> id
    %% kw "WITH"
    -- ws
    -- +.((kw "RECURSIVE" .>> ws) * zeroOrOne)
    -- +.tableName
    -- ws
    -- +.(columnNames * zeroOrOne)
    -- kw "AS"
    -- ws
    -- '('
    -- +.selectStmt
    -- ')'
    -- ws
    -%> fun recurs cteName columnNames asSelect ->
        { Name = cteName; Recursive = Option.isSome recurs; ColumnNames = columnNames; AsSelect = asSelect }

let asAlias =
    %% ((kw "AS" .>> ws) * zeroOrOne)
    -- +.name
    -%> auto

let resultColumn =
    %% +.[
        %% '*' -%> ColumnsWildcard
        %% +.tableName -- '.' -? '*' -%> TableColumnsWildcard
        %% +.expr -- +.(asAlias * zeroOrOne) -%> fun ex alias -> Column (ex, alias)
    ] -- ws -%> auto

let selectColumns =
    %% kw "SELECT"
    -- ws
    -- +.[
            %% kw "DISTINCT" -- ws -%> Some DistinctColumns
            %% kw "ALL" -- ws -%> Some AllColumns
            preturn None
        ]
    -- +.(qty.[1..] / ',' * resultColumn)
    -%> auto

let tableOrSubquery (tableExpr : Parser<TableExpr, unit>) =
    let indexHint =
        %[
            %% kw "INDEXED" -- ws -- kw "BY" -- ws -- +.indexName -%> IndexedBy
            %% kw "NOT" -- ws -- kw "INDEXED" -%> NotIndexed
        ]
    let subterm =
        %[
            %% +.selectStmt -%> fun select alias -> TableOrSubquery (Subquery (select, alias))
            %% +.tableExpr -%> fun table alias -> AliasedTableExpr (table, alias)
        ]
    %[
        %% +.tableInvocation -- +.(asAlias * zeroOrOne) -- +.(indexHint * zeroOrOne)
            -%> fun table alias indexed -> TableOrSubquery (Table (table, alias, indexed))
        %% '(' -- ws -- +.subterm -- ')' -- ws -- +.(asAlias * zeroOrOne) -%> (<|)
    ]

let joinType =
    %[
        %% kw "LEFT" -- ws -- (kw "OUTER" * zeroOrOne) -%> LeftOuter
        %% kw "INNER" -%> Inner
        %% kw "CROSS" -%> Cross
        %% ws -%> Inner
    ]

let joinConstraint =
    %[
        %% kw "ON" -- ws -- +.expr -- ws -%> JoinOn
        %% kw "USING" -- ws -- '(' -- ws -- +.(qty.[1..] / ',' * (columnName .>> ws)) -- ')' -- ws
            -%> fun cols -> JoinUsing (List.ofSeq cols)
        preturn JoinUnconstrained
    ]

let tableExpr =
    precursive <| fun tableExpr ->
        let term = tableOrSubquery tableExpr 
        let natural = %% kw "NATURAL" -- ws -%> ()   
        let join =
            %% +.[
                    %% ',' -%> fun left right constr -> Join (Inner, left, right, constr)
                    %% +.(natural * zeroOrOne) -- +.joinType -- kw "JOIN" -- ws
                        -%> fun natural join left right constr ->
                            let joinType = if Option.isSome natural then Natural join else join
                            Join (joinType, left, right, constr)
                ]
            -- ws
            -- +.term
            -- ws
            -- +.joinConstraint
            -%> fun f joinTo joinOn left -> f left joinTo joinOn
        %% +.term
        -- +.(join * qty.[0..])
        -%> Seq.fold (|>)

let valuesClause =
    %% kw "VALUES"
    -- ws
    -- '('
    -- +.(qty.[0..] / ',' * expr)
    -- ')'
    -%> auto