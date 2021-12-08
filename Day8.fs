module Day8
open AoCFile





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
    0