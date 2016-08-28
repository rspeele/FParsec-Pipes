namespace Examples.SQLite.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open FParsec.Pipes
open Examples.SQLite

[<TestClass>]
type Tests() =
    [<TestMethod>]
    member __.TestSimpleSelect() =
        match run SQLiteParser.stmts @"select * from users u where u.id = 1" with
        | Success(result, _, _) -> printf "%A" result
        | Failure(msg, _, _) -> failwithf "Failure: %s" msg
    [<TestMethod>]
    member __.TestSlightlyFancierSelect() =
        match run SQLiteParser.stmts @"
            select * from users u left join usergroupmaps ugm on ugm.userid = u.id
            where u.id = 1
            and u.name = 'bob'
            group by u.id having u.name = 'jim'
            order by u.name desc
            limit 5 offset 25
        " with
        | Success(result, _, _) -> printf "%A" result
        | Failure(msg, _, _) -> failwithf "Failure: %s" msg