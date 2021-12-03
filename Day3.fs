module Day3
open AoCFile

let TestInput = List.toSeq ["00100"; "00100"; "11110";"10110";"10111";"10101";"01111";"00111";"11100";"10000";"11001";"00010";"01010"]

let Integerize (row:string) = Seq.map (fun c -> int c - int '0') row
let CombineRows row1 row2 = Seq.map2 (fun a b -> a+b) row1 row2

// Most common bits are set to 1
let calcGammaBits data reqSize =   Seq.map (fun v -> if v < reqSize then 0 else 1) data
// Least common bits are set to 1
let calcEpsilonBits data reqSize = Seq.map (fun v -> if v >= reqSize then 0 else 1) data
// Convert sequence of 1's and 0's to decimal number
let BitSeqToDec bitSeq = fst (Seq.foldBack (fun (value:int) (acc:int,idx:int) -> (acc + (value <<< idx), idx+1)) bitSeq (0, 0))

let ExecutePart1 input =
    let rowSum = Seq.fold (fun accumulated row -> CombineRows accumulated (Integerize row)) (Integerize (Seq.head input)) (Seq.tail input)
    let inputLength = Seq.length input
    let gammaRate   =  BitSeqToDec (calcGammaBits rowSum (inputLength/2))
    let epsilonRate = BitSeqToDec (calcEpsilonBits rowSum (inputLength/2))
    printfn "Result Gamma: %d" gammaRate
    printfn "Result Epsilon: %d" epsilonRate
    printfn "Day 3, Part 1: %d" (gammaRate * epsilonRate)

let Execute withFileInput = 
    let input = if withFileInput then (GetInput 3) else TestInput
    ExecutePart1 input