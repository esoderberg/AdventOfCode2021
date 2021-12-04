// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Reflection


let moduleInfo day = 
    Assembly.GetExecutingAssembly().GetTypes()
    |> Seq.find (fun t -> t.Name = "Day"+day)

let ExecuteDay day withFileInput = (moduleInfo day).GetMethod("Execute").Invoke(null, [|withFileInput|])

[<EntryPoint>]
let main argv = 
    let _ = 
        if argv.Length = 0 then
            printf "Enter day number: "
            let day = System.Console.In.ReadLine()
            printf "Press enter for file input or put any character for test input: "
            let inputChoice = (System.Console.In.ReadLine())
            let useFileInput = if inputChoice ="" then true else false
            ExecuteDay day useFileInput
        else
            ExecuteDay argv.[0] (if argv.Length > 1 then true else false)
    0 // return an integer exit code