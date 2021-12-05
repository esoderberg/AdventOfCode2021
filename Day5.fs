module Day5
open AoCFile
open System.Text.RegularExpressions
open System.Collections.Generic
open System

let Range a b = 
    if a <= b then [for v in a..b -> v]
    else [for v in a .. -1 .. b -> v]

type LineSegment =
    {
    x1:int
    y1:int
    x2:int
    y2:int
    }

    member this.IsVertical = this.x1 = this.x2
    member this.IsHorizontal = this.y1 = this.y2
    static member GetCoveredCells (ls: LineSegment) =
        if ls.IsVertical then
            [for y in Range ls.y1 ls.y2 do (ls.x1, y)]
        elif ls.IsHorizontal then
            [for x in Range ls.x1 ls.x2 do yield (x, ls.y1)]
        else // Diagonal
            let xr = (Range ls.x1 ls.x2)
            let yr = (Range ls.y1 ls.y2)
            [for (x,y) in (List.zip xr yr) -> (x,y)]


let lineSegRegex = Regex("(\d+),(\d+) -> (\d+),(\d+)")

let ParseLine (line:string) =  
    let groups = List.map int (List.tail [for g in lineSegRegex.Match(line).Groups -> g.Value]) 
    match groups with
    |   [x1;y1;x2;y2] -> Some({x1=x1; y1=y1; x2=x2; y2=y2})
    | _ -> None
    
let ParseInput input =
    (Seq.map (fun line -> ParseLine line) input)
    |> Seq.where Option.isSome
    |> Seq.map Option.get

let MapAddOverlap cell map= 
    let (key, value) = 
        match Map.tryFind cell map with
        | Some(v) -> (cell,v+1)
        | None -> (cell,1)
    Map.add key value map

let DictAddOverlap cell (map :Dictionary<int*int,int>)= 
    map.[cell] <- map.GetValueOrDefault(cell, 0) + 1
    map

let CalculateOverlaps (cells:(int*int) list) =
    let rec innerCalculateOverlaps (remCells:(int*int) list) overlapMap =
        match remCells with 
            |head::tail -> innerCalculateOverlaps tail (MapAddOverlap head overlapMap)
            |_ -> overlapMap
    innerCalculateOverlaps cells (Map<int*int,int> [])

let CalculateOverlapsDict (cells:(int*int) list) =
    let rec innerCalculateOverlaps (remCells:(int*int) list) overlapMap =
        match remCells with 
            |head::tail -> innerCalculateOverlaps tail (DictAddOverlap head overlapMap)
            |_ -> overlapMap
    innerCalculateOverlaps cells (Dictionary<int*int,int> [])

let PrintOverlapMap map =
    let mapArr = Map.toArray map
    let (minx,miny,maxx,maxy) = 
        Array.fold 
            (fun (minx,miny,maxx,maxy) ((x,y),v) -> ((min minx x), (min miny y), (max maxx x), (max maxy y))) 
            (Int32.MaxValue,Int32.MaxValue, Int32.MinValue, Int32.MinValue)
            mapArr
    for y in miny..maxy do
        for x in minx..maxx do
            match Map.tryFind (x,y) map with
            | Some(v) -> printf "%d" v
            | _ -> printf "."
        printfn ""
    
let ExecutePart1 (lineSegments:LineSegment list) = 
    let chosenLines = List.where (fun (line: LineSegment) -> line.IsHorizontal || line.IsVertical) lineSegments
    let cells = (List.collect LineSegment.GetCoveredCells chosenLines)
    let overlapMap = CalculateOverlaps cells
    //PrintOverlapMap overlapMap // For debugging purposes, not very useful on the real input
    let overlaps =  Map.count (Map.filter (fun k v -> v > 1) overlapMap)
    printfn "Day 5, Part 1: %d" overlaps
   
// Uses a Map to calculate overlaps
let ExecutePart2 (lineSegments:LineSegment list) = 
    let cells = (List.collect LineSegment.GetCoveredCells lineSegments)
    let overlapMap = CalculateOverlaps cells
    //PrintOverlapMap overlapMap
    let overlaps = Map.count (Map.filter (fun k v -> v > 1) overlapMap)
    printfn "Day 5, Part 2: %d" overlaps

// Uses List.countBy for overlaps
let ExecutePart2Alt (lineSegments:LineSegment list) =
    let cells = List.collect LineSegment.GetCoveredCells lineSegments
    let overlapMap = List.countBy id cells
    let overlaps = List.length ((List.filter (fun (k,v) -> v > 1) overlapMap))
    printfn "Day 5, Part 2: %d" overlaps

// Uses a dictionary to calculate overlaps
let ExecutePart2Alt2 (lineSegments:LineSegment list) =
    let cells = List.collect LineSegment.GetCoveredCells lineSegments
    let overlapMap = CalculateOverlapsDict cells
    let overlaps = List.length [for kvpair in overlapMap do if kvpair.Value > 1 then yield kvpair.Key ]
    printfn "Day 5, Part 2: %d" overlaps

let Execute withFileInput = 
    let input = if withFileInput then GetInput 5 else GetTestInput 5
    let lineSegments = List.ofSeq (ParseInput input)
    ExecutePart1 lineSegments
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    ExecutePart2 lineSegments
    stopwatch.Stop()
    printfn "Part2 Map variant ms: %d" stopwatch.ElapsedMilliseconds

    stopwatch.Restart()
    ExecutePart2Alt lineSegments
    stopwatch.Stop()
    printfn "Part2 CountBy variant ms: %d" stopwatch.ElapsedMilliseconds

    stopwatch.Restart()
    ExecutePart2Alt2 lineSegments
    stopwatch.Stop()
    printfn "Part2 dict variant ms: %d" stopwatch.ElapsedMilliseconds
