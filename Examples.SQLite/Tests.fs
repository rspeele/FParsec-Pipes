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
    member __.TestOnSelects() =
        for file in Directory.GetFiles("../../Tests", "*.test") do
            printfn "### File: %s ###" (Path.GetFileNameWithoutExtension(file))
            let text =
                File.ReadAllLines(file)
                |> Array.filter (fun s -> not (s.StartsWith("#")))
                |> String.concat Environment.NewLine
            let mutable startIndex = 0
            while startIndex >= 0 do
                let index = text.IndexOf("SELECT ", startIndex, StringComparison.OrdinalIgnoreCase)
                if index < 0 then
                    startIndex <- -1
                else
                    let substr = text.Substring(index)
                    match run SQLiteParser.stmts1 substr with
                    | Success(result, _, _) ->
                        Console.WriteLine(new String('-', 80))
                        printf "%A" result
                        Console.WriteLine(new String('-', 80))
                    | Failure(msg, _, _) -> failwithf "Failure: %s" msg
                    startIndex <- index + 1