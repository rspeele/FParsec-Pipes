(*** hide ***)

#r @"bin/debug/FParsecCS.dll"
#r @"bin/debug/FParsec.dll"
#r @"bin/debug/FParsec-Pipes.dll"
open System
open FParsec
open FParsec.Pipes

// A comment.

let digits (n : int) = %(n, digit) |>> (String >> Int32.Parse)

let date =
    !+ digits(4) -- '-' -+ digits(2) -- '-' -+ digits(2) -%> autofun

let time =
    !+ digits(2) -- ':' -+ digits(2) -- ':' -+ digits(2) -%> autofun

let iso8601 : Parser<_, unit> =
    !+ date -- 'T' -+ time
    -|> fun (yyyy, mm, dd) (hh, mi, ss) ->
        new DateTime(yyyy, mm, dd, hh, mi, ss)