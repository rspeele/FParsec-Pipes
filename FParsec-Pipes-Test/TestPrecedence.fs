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
    | Maybe of Expr
    | Sub of (Expr * Expr)
    | Add of (Expr * Expr)
    | Mul of (Expr * Expr)
    | Pow of (Expr * Expr)
    | Email of (Expr * Expr)
    override this.ToString() =
        match this with
        | Literal i -> string i
        | Name n -> string n
        | Neg e -> "(- " + string e + " -)"
        | Maybe e -> "(? " + string e + " ?)"
        | Sub (l, r) -> "[- " + string l + " " + string r  + " -]"
        | Add (l, r) -> "[+ " + string l + " " + string r  + " +]"
        | Mul (l, r) -> "[* " + string l + " " + string r  + " *]"
        | Pow (l, r) -> "[^ " + string l + " " + string r  + " ^]"
        | Email (l, r) -> "[@ " + string l + " " + string r  + " @]"

[<TestClass>]
type TestPrecedence() =
    static let literal =
        puint32 |>> Literal

    static let name =
        many1Satisfy2 isLetter (fun c -> isDigit c || isLetter c)
        |>> Name

    static let ops =
        [
            [ % '@', infixlt Email ]
            [ % '?', postfix Maybe ]
            [ % '-', prefix Neg ]
            [ % '^', infixrt Pow ]
            [ % '*', infixlt Mul ]
            [ % '-', infixlt Sub; % '+', infixlt Add ]
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
        test "x + y ^ z * q?" "[+ x [* [^ y z ^] (? q ?) *] +]"

    [<TestMethod>]
    member __.TestPrecedenceNoSpaces() =
        test "x+y^z*q?" "[+ x [* [^ y z ^] (? q ?) *] +]"

    [<TestMethod>]
    member __.TestPrecedenceHighestLeft() =
        test "1 @ 2 @ 3" "[@ [@ 1 2 @] 3 @]"
        
