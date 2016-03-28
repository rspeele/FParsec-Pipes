#load "../packages/FSharp.Formatting.2.12.0/FSharp.Formatting.fsx"
open FSharp.Literate
open System.IO

let source = __SOURCE_DIRECTORY__
let template = Path.Combine(source, "template.html")

for file in ["Intro.fsx"] do
    Literate.ProcessScriptFile
        (Path.Combine(source, file), template, lineNumbers = false)