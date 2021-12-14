module Day14
open AoCFile
open System.Collections.Generic


// Parse pairs and return them as ((ch1,ch2),result)
let ParseInsertionPair (pairRaw:string) =
    let parts = pairRaw.Split " -> "
    let (pair, result) = (parts.[0],parts.[1])
    ((pair.[0],pair.[1]), result.[0])

// Steps the template forward one step
let ProccessTemplate (template:char list) (pairMap:IDictionary<(char*char),char>) = 
    let pairs = Seq.toList (Seq.zip template (List.skip 1 template))
    (List.head template)::(List.collect (fun (a,b) -> [pairMap.[(a,b)]; b]) pairs)

let ExecutePart1 template pairMap =
    let result = List.fold (fun t i -> ProccessTemplate t pairMap) template [1..10]
    let occurrances = (List.countBy id result)
    let max = List.maxBy (fun (chr,count) -> count) occurrances
    let min = List.minBy (fun (chr,count) -> count) occurrances
    printfn "Day 14, Part 1: %d" ((snd max) - (snd min))


let Execute withRealInput = 
    let lines = Seq.toList (if withRealInput then GetInput 14 else GetTestInput 14)
    let (template, pairsRaw) = (List.ofSeq (List.head lines), List.tail (List.skip 1 lines))
    let pairMap = dict (List.map ParseInsertionPair pairsRaw)
    ExecutePart1 template pairMap