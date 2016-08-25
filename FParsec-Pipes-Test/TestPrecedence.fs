namespace FParsec.Pipes.Test
open FParsec.Pipes.Test.Tools
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open FParsec.Pipes
open FParsec.Pipes.Precedence

type Expr =
    | Literal of uint32
    | Name of string
    | Neg of Expr
    | Factorial of Expr
    | Sub of (Expr * Expr)
    | Add of (Expr * Expr)
    | Mul of (Expr * Expr)
    | Pow of (Expr * Expr)
    | Email of (Expr * Expr)
    | Conditional of (Expr * Expr * Expr)
    | Between of (Expr * Expr * Expr)
    override this.ToString() =
        match this with
        | Literal i -> string i
        | Name n -> string n
        | Neg e -> "(- " + string e + " -)"
        | Factorial e -> "(! " + string e + " !)"
        | Sub (l, r) -> "[- " + string l + " " + string r  + " -]"
        | Add (l, r) -> "[+ " + string l + " " + string r  + " +]"
        | Mul (l, r) -> "[* " + string l + " " + string r  + " *]"
        | Pow (l, r) -> "[^ " + string l + " " + string r  + " ^]"
        | Email (l, r) -> "[@ " + string l + " " + string r  + " @]"
        | Conditional (cond, l, r) -> "{? " + string cond + " then " + string l + " else " + string r + " ?}"
        | Between (input, low, high) -> "{| " + string input + " between " + string low + " and " + string high + " |}"

[<TestClass>]
type TestPrecedence() =
    static let literal =
        puint32 |>> Literal

    static let name =
        many1Satisfy2 isLetter (fun c -> isDigit c || isLetter c)
        |>> Name

    static let ops =
        [
            [ infixlt '@' Email ]
            [ postfix '!' Factorial ]
            [ prefix '-' Neg ]
            [ infixrt '^' Pow ]
            [ infixlt '*' Mul ]
            [ infixlt '-' Sub; infixlt '+' Add ]
            [ ternarylt "between" "and" Between ]
            [ ternaryrt '?' ':' Conditional ]
        ]
    static let expr =
        {
            Whitespace = spaces
            Term = fun expr -> choice [| literal; name; %% '(' -- spaces -- +.expr -- ')' -%> auto |]
            Operators = ops
        } |> expression

    static let test input expected =
        let parsed = run expr input
        match parsed with
        | Success(result, _, _) ->
            Assert.AreEqual(expected, result.ToString())
        | Failure(msg, _, _) ->
            Assert.Fail(msg)

    [<TestMethod>]
    member __.TestPrecedence() =
        test "x + y ^ z * q!" "[+ x [* [^ y z ^] (! q !) *] +]"

    [<TestMethod>]
    member __.TestPrecedenceNoSpaces() =
        test "x+y^z*q!" "[+ x [* [^ y z ^] (! q !) *] +]"

    [<TestMethod>]
    member __.TestPrecedenceHighestLeft() =
        test "1 @ 2 @ 3" "[@ [@ 1 2 @] 3 @]"

    [<TestMethod>]
    member __.TestLeftAssociativity() =
        test "x + y + z" "[+ [+ x y +] z +]"

    [<TestMethod>]
    member __.TestRightAssociativity() =
        test "x ^ y ^ z" "[^ x [^ y z ^] ^]"

    [<TestMethod>]
    member __.TestConditional() =
        test "x + y ? a?b:c : d?e:f" "{? [+ x y +] then {? a then b else c ?} else {? d then e else f ?} ?}"

    [<TestMethod>]
    member __.TestBetween() =
        test "-x between 0 and 10" "{| (- x -) between 0 and 10 |}"

    [<TestMethod>]
    member __.TestBetweenAssociativity() =
        test "-x between 0 and y between 1 and 2" "{| {| (- x -) between 0 and y |} between 1 and 2 |}"
        
