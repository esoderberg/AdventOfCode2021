module Day13
open AoCFile

type Instruction = { axis:string; value:int }
type Dot = {x:int; y:int}

let ParseInstruction (instructionRaw:string) = 
    let parts = instructionRaw.Split ' '
    let axisParts = parts.[2].Split '='
    let axis = axisParts.[0]
    let value = axisParts.[1]
    {axis = axis; value = int value}

let ParseDot (dotRaw:string) = 
    let parts = dotRaw.Split ','
    {x = int parts.[0]; y = int parts.[1]}

// Folds a dot according to the instruction.
// A dot on the non-folding side of the fold is returned as-is.
let FoldDot (instruction:Instruction) (dot:Dot) = 
    if instruction.axis = "x" then
        if dot.x > instruction.value then
            {x = 2*instruction.value - dot.x; y = dot.y}
        else 
            dot
    else
        if dot.y > instruction.value then
            {x = dot.x; y = 2*instruction.value - dot.y}
        else
            dot

// Returns a list of the dots present on the sheet after folding according to the instruction
let ExecuteInstruction (dots:Dot list) (instruction:Instruction) = List.distinct (List.map (FoldDot instruction) dots)

let ExecutePart1 dots instructions =
    let result = ExecuteInstruction dots (List.head instructions)
    printfn "Day 13, Part 1: %d" result.Length

let ExecutePart2 dots instructions =
    let result = List.fold (fun dots instruction -> ExecuteInstruction dots instruction) dots instructions
    let dotSet = set result 
    let xlim = (List.maxBy (fun dot -> dot.x) result).x
    let ylim = (List.maxBy (fun dot -> dot.y) result).y
    printfn "Day 13, Part 2:"
    for y in 0..ylim do
        for x in 0..xlim do
            printf (if Set.contains {x=x;y=y} dotSet then "#" else ".")
        printfn ""

let Execute withRealInput =
    let lines = if withRealInput then GetInput 13 else GetTestInput 13
    let dotsRaw, instructionsRaw = List.partition (fun (line:string) ->  line.Contains ',') (Seq.toList lines)
    let instructions = List.map ParseInstruction (List.tail instructionsRaw)
    let dots = List.map ParseDot dotsRaw
    ExecutePart1 dots instructions
    ExecutePart2 dots instructions
    ()