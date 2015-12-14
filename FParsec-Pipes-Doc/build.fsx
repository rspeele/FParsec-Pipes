#load "../packages/FSharp.Formatting.2.12.0/FSharp.Formatting.fsx"
open FSharp.Literate
open System.IO

let source = __SOURCE_DIRECTORY__
let template = Path.Combine(source, "template.html")

let script = Path.Combine(source, "Tutorial.fsx")
Literate.ProcessScriptFile(script, template)