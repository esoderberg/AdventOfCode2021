module Day9

open AoCFile


let IsLowestPoint (grid: int [][]) (x,y) = 
    let (xmax,ymax) = (grid.[0].Length-1, grid.Length-1)
    let check_cells = List.filter (fun (x,y) -> 0 <= x && x <= xmax && 0 <= y && y <= ymax) [(x,y-1);(x+1,y);(x,y+1);(x-1,y)]
    let check_values = List.map (fun (cx,cy) -> grid.[cy].[cx]) check_cells
    let center = grid.[y].[x]
    
    if List.exists (fun cv -> cv <= center) check_values then false else true
    

let ExecutePart1 (input: int[][]) = 
    let positions = [|for y in 0..input.Length-1 do for x in 0..input.[0].Length-1 -> (x,y)|]
    let lowCells = Array.filter (IsLowestPoint input) positions
    let riskLevels = Array.map (fun (x,y) -> input.[y].[x]+1) lowCells
    printfn "Day 9, Part 1: %d" (Array.sum riskLevels)


    

let Execute withRealFile  =
    let lines = if withRealFile then GetInput 9 else GetTestInput 9
    let input = Array.ofSeq (Seq.map (fun line -> Array.ofSeq (Seq.map (fun c -> int c - int '0') line)) lines)
    ExecutePart1 input