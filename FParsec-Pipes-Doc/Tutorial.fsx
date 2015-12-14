#r @"bin/debug/FParsecCS.dll"
#r @"bin/debug/FParsec.dll"
#r @"bin/debug/FParsec-Pipes.dll"
open System
open FParsec
open FParsec.Pipes

// A comment.

let digits (n : int) = %(n, digit) |>> (String >> Int32.Parse)

type Date = { Year : int; Month : int; Day : int }

let dateShape : Parser<_, unit> =
    !- digits(4) -- '-' -- digits(2) -- '-' -- digits(2) -|> ()

let date : Parser<Date, unit> =
    !+ digits(4) -- '-' -+ digits(2) -- '-' -+ digits(2)
    -|> fun yyyy mm dd -> { Year = yyyy; Month = mm; Day = dd }