namespace Examples.SQLite.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open FParsec.Pipes
open Examples.SQLite

[<TestClass>]
type Tests() =
    static let test sql =
        match run (SQLiteParser.stmts .>> eof) sql with
        | Success(result, _, _) -> printf "%A" result
        | Failure(msg, _, _) -> failwithf "Failure: %s" msg
    [<TestMethod>]
    member __.TestSimpleSelect() =
        test @"select * from users u where u.id = 1"

    [<TestMethod>]
    member __.TestSlightlyFancierSelect() =
        test @"
            select * from users u left join usergroupmaps ugm on ugm.userid = u.id
            where u.id = 1
            and u.name = 'bob'
            group by u.id having u.name = 'jim'
            order by u.name desc
            limit 5 offset 25
        "

    [<TestMethod>]
    member __.TestVariousJoinTypes() =
        test @"
            select * from users
            left join usergroupmaps ugm on ugm.userid = userid
            inner join foo f on f.id = userid
            cross join bar b on b.id <> f.id and b.name like '%stuff_%' escape '_'
            where u.id > 5
            group by u.id, u.name having u.name = 'jim'
            order by u.name desc
            limit 5
        "