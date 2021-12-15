module Day15
open AoCFile
open GridUtil






let FindShortestPath grid startCell endCell = 
    let (mx,my) = GridUtil.GridBounds grid
    let dist = [|for y in 0..my -> [|for x in 0..mx -> System.Int32.MaxValue|]|]
    dist.[snd startCell].[fst startCell] <- 0
    let prev = [|for y in 0..my -> [|for x in 0..mx -> (-1,-1)|]|]
    let mutable Q = Set [|for y in 0..my do for x in 0..mx -> (x,y)|] 

    // I just did not have it in me to try to do a non-mutable version 

    while not Q.IsEmpty do
        let (x, y) = Array.minBy (fun (x,y) -> dist.[y].[x]) (Array.ofSeq Q)
        Q <- Set.remove (x,y) Q
        for (u,v) in List.filter (fun cell -> Set.contains cell Q) (GridUtil.GetCellPlus grid (x,y)) do
            let alt = dist.[y].[x] + grid.[v].[u]
            if alt < dist.[v].[u] then
                dist.[v].[u] <- alt
                prev.[v].[u] <- (x,y)
    
    let mutable path = []
    let mutable u = endCell
    if prev.[snd u].[fst u] <> (-1,-1) || endCell = startCell then
        while u <> (-1,-1) do
           path <- u::path
           u <- prev.[snd u].[fst u]

    path


    

let ExecutePart1 grid =
    let topLeft = (0,0)
    let bottomRight = GridUtil.GridBounds grid
    let shortestPath = FindShortestPath grid topLeft bottomRight
    let risk = (List.sum (List.map (fun (x,y) -> grid.[y].[x]) shortestPath)) - (grid.[0].[0])
    printfn "Day 15, Part 1: %d" risk

    ()


let Execute withRealInput = 
    let lines = if withRealInput then GetInput 15 else GetTestInput 15
    let grid = [|for line in lines -> Array.map (fun c -> (int c) - int '0') (Array.ofSeq line)|]
    ExecutePart1 grid
    ()
