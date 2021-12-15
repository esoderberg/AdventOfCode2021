module Day15
open AoCFile
open GridUtil






let FindShortestPath grid startCell endCell = 
    let (mx,my) = GridUtil.GridBounds grid
    let dist = [|for y in 0..my -> [|for x in 0..mx -> System.Int32.MaxValue|]|]
    dist.[snd startCell].[fst startCell] <- 0
    let prev = [|for y in 0..my -> [|for x in 0..mx -> (-1,-1)|]|]

    let mutable unvisitedWithDistance = Set [startCell]
    let mutable visited = Set []
    // I just did not have it in me to try to do a non-mutable version 
    
    while not unvisitedWithDistance.IsEmpty do
        let (x, y) = Array.minBy (fun (x,y) -> dist.[y].[x]) (Array.ofSeq (unvisitedWithDistance)) // Pretty sure this is the slowest part
        visited <- Set.add (x,y) visited
        unvisitedWithDistance <- Set.remove (x,y) unvisitedWithDistance
        if (x,y) = endCell then unvisitedWithDistance <- Set []
        else
            for (u,v) in List.filter (fun cell -> not (Set.contains cell visited)) (GridUtil.GetCellPlus grid (x,y)) do
                let alt = dist.[y].[x] + grid.[v].[u]
                if dist.[v].[u] = System.Int32.MaxValue then
                    unvisitedWithDistance <- Set.add (u,v) unvisitedWithDistance
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

let IncreaseCell (gx,gy) row col cell = 
    let gridy = row/gy // Which grid row we're in
    let gridx = col/gx // which grid column we're in
    let res = (cell + gridy + gridx)
    if res <= 9 then res
    else (res % 10) + 1
    

let ExecutePart2 grid =
    let topLeft = (0,0)
    let bottomRight = (GridUtil.GridBounds grid)
    let bottomRight = ((fst bottomRight)+1, (snd bottomRight)+1)
    let bigGrid = Array.map (fun row -> Array.concat (seq {for i in 1..5 -> row })) grid // Enlarge horizontally
    let bigGrid = Array.concat (seq {for i in 1..5 -> bigGrid}) // Enlare vertically 
    let bigGrid = Array.mapi (fun i row -> Array.mapi (IncreaseCell bottomRight i) row) bigGrid // Fix risk increments
    let bottomRight = (GridUtil.GridBounds bigGrid)
    let shortestPath = FindShortestPath bigGrid topLeft bottomRight
    let risk = (List.sum (List.map (fun (x,y) -> bigGrid.[y].[x]) shortestPath)) - (bigGrid.[0].[0])
    printfn "Day 15, Part 2: %d" risk

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
    ExecutePart2 grid
    ()
