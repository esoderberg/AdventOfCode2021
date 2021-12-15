module Day14
open AoCFile
open System.Collections.Generic


// Parse pairs and return them as ((ch1,ch2),result)
let ParseInsertionPair (pairRaw:string) =
    let parts = pairRaw.Split " -> "
    let (pair, result) = (parts.[0],parts.[1])
    ((pair.[0],pair.[1]), result.[0])

// Steps the template forward one step
let ProcessTemplate (template:char list) (pairMap:IDictionary<(char*char),char>) = 
    let pairs = Seq.toList (Seq.zip template (List.skip 1 template))
    (List.head template)::(List.collect (fun (a,b) -> [pairMap.[(a,b)]; b]) pairs)

let ExecutePart1 template pairMap =
    let result = List.fold (fun t i -> ProcessTemplate t pairMap) template [1..10]
    let occurrances = (List.countBy id result)
    let max = List.maxBy (fun (chr,count) -> count) occurrances
    let min = List.minBy (fun (chr,count) -> count) occurrances
    printfn "Day 14, Part 1: %d" ((snd max) - (snd min))


let FastProcessTemplate (pairs: Dictionary<(char*char),bigint>) (letterCount: Dictionary<char,bigint>) (pairMap:IDictionary<(char*char),char>) =
    let newPairs = Dictionary<(char*char),bigint> []
    let newLetterCount = Dictionary<char,bigint> letterCount
    Seq.iter (fun ((ch1,ch2), count) ->
        let insert = pairMap.[(ch1,ch2)]
        let p1 = (ch1, insert)
        let p2 = (insert, ch2)
        let v1 = newPairs.GetValueOrDefault(p1,bigint.Zero)
        let v2 = newPairs.GetValueOrDefault(p2,bigint.Zero)
        if(count > bigint.Zero) then
            newPairs.[p1] <-  v1 + count
            newPairs.[p2] <-  v2 + count
            newLetterCount.[insert] <- newLetterCount.GetValueOrDefault(insert, bigint.Zero) + count
            
        ) ([for entry in pairs do (entry.Key, entry.Value)])

    (newPairs, newLetterCount)

let ExecutePart2 (template:char list) (pairMap: IDictionary<(char * char),char>) =
    let startingPairs = Seq.toList (Seq.zip template (List.tail template))
    let pairCounts = Dictionary<(char*char),bigint> []
    let letterCounts =  Dictionary<char,bigint> []

    List.iter  (fun pair -> pairCounts.[pair] <- pairCounts.GetValueOrDefault(pair,bigint.Zero)+bigint.One) (startingPairs)
    List.iter (fun c -> letterCounts.[c] <- letterCounts.GetValueOrDefault(c,bigint.Zero)+bigint.One) template

    let (resultPairs,resultLetters) = List.fold (fun (t,l) i -> FastProcessTemplate t l pairMap) (pairCounts, letterCounts) [1..1000]

    let lettersList = [for entry in resultLetters do (entry.Key, entry.Value)]
    let max = List.maxBy (fun (chr,count) -> count) lettersList
    let min = List.minBy (fun (chr,count) -> count) lettersList
    printfn "Day 14, Part 2: %s" (((snd max) - (snd min)).ToString())

let Execute withRealInput = 
    let lines = Seq.toList (if withRealInput then GetInput 14 else GetTestInput 14)
    let (template, pairsRaw) = (List.ofSeq (List.head lines), List.tail (List.skip 1 lines))
    let pairMap = dict (List.map ParseInsertionPair pairsRaw)
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    ExecutePart1 template pairMap
    ExecutePart2 template pairMap
    stopwatch.Stop()
    printfn "Time: %dms" stopwatch.ElapsedMilliseconds