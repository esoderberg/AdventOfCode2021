module Day1

open System.Collections


let FileInput = Seq.map (fun line -> int line) (System.IO.File.ReadLines "Input/Day1.txt")
let TestInput = [199; 200; 208;210;200;207;240;269;260;263]

let CountIncreases pairs = Seq.fold (fun acc (prev,curr) -> acc + if prev < curr then 1 else 0) 0 pairs

let ExecutePart1 input = 
    let zipped = (Seq.zip input (Seq.skip 1 input))
    printfn "Day 1, Part 1: %d" (CountIncreases zipped)

let ExecutePart2 input =
    let zipped = (Seq.zip3 input (Seq.skip 1 input) (Seq.skip 2 input))
    let three_measure_windows = Seq.map (fun (a,b,c) -> a+b+c) zipped
    let zipped_windows = Seq.zip three_measure_windows (Seq.skip 1 three_measure_windows)
    printfn "Day 1, Part 2: %d" (CountIncreases zipped_windows)

let Execute = 
    ExecutePart1 FileInput
    ExecutePart2 FileInput