module Day11

open AoCFile
open GridUtil



let rec FlashOctopus grid (x,y) =
    let adjacent = GetCellSquare grid (x,y)
    grid.[y].[x] <- 0
    List.iter 
        (fun (x,y) -> 
            if grid.[y].[x] > 0 then 
                grid.[y].[x] <- grid.[y].[x] + 1
                if grid.[y].[x] > 9 then
                    FlashOctopus grid (x,y)
        )
        adjacent

let SimulateStep grid = 
    let newGrid = Array.map (fun row -> Array.map (fun energy -> energy+1) row) grid
    let (xmax,ymax) = GridBounds newGrid
    for y in 0..ymax do
        for x in 0..xmax do
            if newGrid.[y].[x] > 9 then
                FlashOctopus newGrid (x,y)
                
    newGrid

let rec SimulateSteps grid N =
    if N = 0 then []
    else 
        let newGrid = SimulateStep grid
        newGrid::(SimulateSteps newGrid (N-1))

let PrintGrid (grid:int[][]) =
    Array.iter (fun row -> printfn "%s" (String.concat "" (Array.map string row))) grid
    printfn ""

let CountJustFlashed grid = 
    let flashed = Array.collect (fun row -> Array.map (fun elem -> if elem = 0 then 1 else 0) row ) grid
    Array.sum flashed

let ExecutePart1 grid = 
    let simulationSteps = grid::(SimulateSteps grid 100)
    List.iter PrintGrid simulationSteps
    let flashes = List.map CountJustFlashed simulationSteps
    printfn "Day 11, Part 1: %d" (List.sum flashes)
    //List.iter (fun (f,v) -> printfn "%d" v) flashes
    ()

let Execute withRealInput = 
    let input = if withRealInput then GetInput 11 else GetTestInput 11
    let grid = Array.ofSeq (Seq.map (fun str -> Array.ofSeq (Seq.map (fun c -> (int c) - (int '0')) str)) input)
    ExecutePart1 grid
    ()