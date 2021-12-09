module Day9

open AoCFile

// Gets the cells in a + pattern with x,y in the center (not in the list). 
// if a cell would go below zero or above it's respective max it will not be included.
let IsCellInGrid (xmax,ymax) (x,y) =  0 <= x && x <= xmax && 0 <= y && y <= ymax
let GetCellPlus (x,y) (xmax,ymax) = List.filter (IsCellInGrid (xmax,ymax)) [(x,y-1);(x+1,y);(x,y+1);(x-1,y)]

let rec GetBasins (grid: int [][]) (x,y) = 
    let (xmax,ymax) = (grid.[0].Length-1, grid.Length-1)
    let check_cells = GetCellPlus (x,y) (xmax,ymax)
    let value = grid.[y].[x]
    let kept_cells = List.filter (fun (cx,cy) -> grid.[cy].[cx] > value && grid.[cy].[cx] < 9) check_cells
    //printfn "value: %d" value
    //List.iter (fun (cx,cy) -> printf "(%d,%d)=%d " cx  cy grid.[cy].[cx]) kept_cells
    //printfn "\n--------------" 
    (x,y)::(List.collect (fun (cx,cy) -> GetBasins grid (cx,cy)) kept_cells)
    

let IsLowestPoint (grid: int [][]) (x,y) = 
    let (xmax,ymax) = (grid.[0].Length-1, grid.Length-1)
    let check_cells = GetCellPlus (x,y) (xmax,ymax)
    let check_values = List.map (fun (cx,cy) -> grid.[cy].[cx]) check_cells
    let center = grid.[y].[x]
    
    if List.exists (fun cv -> cv <= center) check_values then false else true
    

let ExecutePart1 (input: int[][]) = 
    let positions = [|for y in 0..input.Length-1 do for x in 0..input.[0].Length-1 -> (x,y)|]
    let lowCells = Array.filter (IsLowestPoint input) positions
    let riskLevels = Array.map (fun (x,y) -> input.[y].[x]+1) lowCells
    printfn "Day 9, Part 1: %d" (Array.sum riskLevels)
    lowCells


let ExecutePart2 (input: int[][]) (lowPoints: (int*int) []) = 
    let basins = Array.map (fun lp -> GetBasins input lp |> List.distinct) lowPoints
    let sortedBasins = Array.sortByDescending (fun (b: (int * int) list) -> b.Length) basins
    let threeLargest = Array.take 3 sortedBasins
    printfn "Day 9, Part 2: %d" (Array.fold (fun m (b: (int * int) list) -> m*b.Length) 1 threeLargest)
    

let Execute withRealFile  =
    let lines = if withRealFile then GetInput 9 else GetTestInput 9
    let input = Array.ofSeq (Seq.map (fun line -> Array.ofSeq (Seq.map (fun c -> int c - int '0') line)) lines)
    let lowpoints = ExecutePart1 input
    ExecutePart2 input lowpoints