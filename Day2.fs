module Day2

let FileInput = Seq.cache (System.IO.File.ReadLines "Input/Day2.txt")
let TestInput = List.toSeq ["forward 5"; "down 5"; "forward 8"; "up 3"; "down 8"; "forward 2"];

let ParseCommand (cmd : string) = 
    let parts = cmd.Split ' '
    (parts.[0], int parts.[1])

let SimpleCommandInterpreter (hor, ver) cmd =
    match cmd with
    | ("forward",value) -> (hor+value, ver)
    | ("down",value)    -> (hor, ver+value)
    | ("up",value)      -> (hor, ver-value)
    | (_,_)             -> (hor, ver)

let AdvancedCommmandInterpreter (hor, ver, aim) cmd =
    match cmd with
    | ("forward",value) -> (hor+value, ver + aim*value, aim)
    | ("down",value)    -> (hor, ver, aim+value)
    | ("up",value)      -> (hor, ver, aim-value)
    | (_,_)             -> (hor, ver, aim)
    
let CalculateDestinationCoordinates position commandInterpreter commandSeq resultCalculator= 
    Seq.fold (fun position cmd -> commandInterpreter position cmd) position commandSeq
    |> resultCalculator
 
let Run commandInterpreter startpos input = CalculateDestinationCoordinates startpos commandInterpreter input

let Execute useFile = 
    let commands = Seq.map ParseCommand (if useFile then FileInput else TestInput)
    let result_day1 = Run SimpleCommandInterpreter (0,0) commands (fun (hor,ver) -> hor*ver)
    let result_day2 = Run AdvancedCommmandInterpreter (0,0,0) commands (fun (hor,ver,_) -> hor*ver)
    printfn "Day 2, Part 1: %d" result_day1
    printfn "Day 2, Part 2: %d" result_day2