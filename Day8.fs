module Day8
open AoCFile



// This looks pretty dumb but might be kinda fast?
// Could probably be even faster if you assigned each character a bit value
// and through that gave each digit an unique number then one could use bitoperators for comparisons.

let SolveZeroNineSix (zeroSixNine:Set<char>[]) four one = 
    let (a, b, c) = (zeroSixNine.[0], zeroSixNine.[1], zeroSixNine.[2])
    let (Six,a,b) = 
        if (a-one).Count = 5 then (a,b,c)
        elif (b-one).Count = 5 then (b,a,c)
        else (c,a,b)
    let (Zero,Nine) = if (a-four).Count = 3 then (a,b) else (b,a)
    (Zero,Six,Nine)


let SolveTwoThreeFive (twoThreeFive:Set<char>[]) four = 
    let (a, b, c) = (twoThreeFive.[0],twoThreeFive.[1],twoThreeFive.[2])
    let (Two,a,b) = if (a-four).Count = 3 then (a,b,c) elif (b-four).Count = 3 then (b,a,c) else (c,a,b)
    let (Three,Five) = if (a-Two).Count = 1 then (a,b) else (b,a)
    (Two,Three,Five)


let Solve (signals:string[], digits:string[]) =
    // Mapping of segments -> set of characters
    let sets = Array.map (fun (s: string) -> set s) signals
    let digitSets = Array.map (fun (s: string) -> set s) digits
    // A little inefficient 
    let one = Array.find (fun s -> Set.count s = 2) sets
    let four = Array.find (fun s -> Set.count s = 4) sets
    let seven = Array.find (fun s -> Set.count s = 3) sets
    let eight = Array.find (fun s -> Set.count s = 7) sets
    let ZeroSixNine = Array.where (fun s -> Set.count s = 6) sets
    let TwoThreeFive = Array.where (fun s -> Set.count s = 5) sets
    let (zero, six, nine) = SolveZeroNineSix ZeroSixNine four one
    let (two,three,five) = SolveTwoThreeFive TwoThreeFive four
    let NumberSets = [|(zero,0); (one, 1); (two, 2); (three, 3); (four, 4); (five, 5); (six, 6); (seven, 7); (eight, 8); (nine, 9)|]
    let numbers = Array.map (fun digit -> snd (Array.find (fun (s,n) -> s = digit) NumberSets)) digitSets
    Array.fold (fun num n -> (num*10+n)) 0 numbers
    
    
    


let ExecutePart2 (input:(string[]*string[]) list) = 
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let solved = List.map Solve input
    let sum = List.sum solved
    // List.iter (fun s -> printfn "%d" s) solved
    printfn "Day 8, Part 2: %d" sum
    stopwatch.Stop()
    printfn "Time: %dms" stopwatch.ElapsedMilliseconds

let ExecutePart1 (input:(string[]*string[]) list) = 
    let digits = List.map (fun i -> snd i) input
    let digits_flat = Array.concat digits
    let segmentCount = dict (Array.countBy (fun (digit: string) -> digit.Length) digits_flat)
    printfn "Day 8, Part 1: %d" (segmentCount.[2]+segmentCount.[3]+segmentCount.[4]+segmentCount.[7])
    


let ParseLine (line: string) = 
    let splitLine = (line.Split " | ")
    let signals = splitLine.[0].Split ' '
    let digits = splitLine.[1].Split ' '
    (signals, digits)

let Execute withRealInput = 
    let input = List.ofSeq (if withRealInput then GetInput 8 else GetTestInput 8)
    let splitinput =  List.map ParseLine input
    ExecutePart1 splitinput
    ExecutePart2 splitinput
    0