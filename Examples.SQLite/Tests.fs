namespace Examples.SQLite.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open System
open System.IO
open FParsec
open FParsec.Pipes
open Examples.SQLite

[<TestClass>]
type Tests() =
    static let test sql =
        match run (SQLiteParser.stmts .>> eof) sql with
        | Success(result, _, _) -> printf "%A" result
        | Failure(msg, _, _) -> failwithf "Failure: %s" msg

    static let testFile fileName =
        let sql = File.ReadAllText(fileName)
        let fileName = Path.GetFileName(fileName)
        let failure =
            try
                match runParserOnString (SQLiteParser.stmts .>> eof) () fileName sql with
                | Success(result, _, _) -> None
                | Failure(msg, _, _) -> Some (sprintf "Failure: %s" msg)
            with
            | exn -> failwithf "Exception in %s (%O)" fileName exn
        match failure with
        | None -> ()
        | Some fail -> failwith fail

    [<TestMethod>]
    [<Ignore>]
    member __.TestSqlFiles() =
        for file in Directory.GetFiles(@"TestFiles", "*.sql") do
            testFile file

    [<TestMethod>]
    member __.TestNotIn() =
        test @"select * from users where x > 5 and y not in (a,b,c)"

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
    member __.TestMultipleColumns() =
        test @"select u.id , 1+max(1,y) , u.* , func(x) from users u where u.id = 1"

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

    [<TestMethod>]
    member __.TestCompound() =
        test @"
            SELECT a AS x, sum(b) AS y FROM t1 GROUP BY a
            UNION
            SELECT 98 AS x, 99 AS y
        "

    [<TestMethod>]
    member __.TestCreate() =
        test @"
            create table main.x 
            ( id int not null primary key
            , name clob
            , friendid id references main.y(id)
            ) 
        "

    [<TestMethod>]
    member __.TestCreateTemp() =
        test @"
            create temp table x as
            select a, b, c from main.realtable
            where a > 5
            limit 1
        "

    [<TestMethod>]
    member __.TestCreateWithBounds() =
        test @"
            create table tbl(x varchar(255), y numeric( 1 , 3 ))
        "

    [<TestMethod>]
    member __.TestOnSelects() =
        let parser = SQLiteParser.selectStmt .>> SQLiteParser.ws .>> %[';'; '}'; ')'; '"']
        let mutable total = 0
        for file in Directory.GetFiles("./Tests", "*.test") do
            let mutable good = 0
            let name = Path.GetFileNameWithoutExtension(file)
            printfn "### File: %s ###" name
            let text =
                File.ReadAllLines(file)
                |> Array.filter (fun s -> not (s.TrimStart().StartsWith("#")))
                |> String.concat Environment.NewLine
            let mutable startIndex = 0
            while startIndex >= 0 do
                let index = text.IndexOf("SELECT ", startIndex, StringComparison.OrdinalIgnoreCase)
                if index < 0 then
                    startIndex <- -1
                else
                    let substr = text.Substring(index)
                    match run parser substr with
                    | Success(result, _, _) ->
                        good <- good + 1
                    | Failure(msg, _, _) ->
                        let truncated = if substr.Length > 120 then substr.Substring(0, 120) else substr
                        failwithf "Failure (after %d successes). In %s:\r\n%s\r\n (context: %s)" good name msg truncated
                    startIndex <- index + 1
            printfn "Parsed %d SELECT statements" good
            total <- total + good
        printfn "Parsed %d total SELECT statements" total