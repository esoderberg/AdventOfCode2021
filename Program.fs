﻿// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Day1
// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv = 
    let d1 = Day1.Execute
    0 // return an integer exit code