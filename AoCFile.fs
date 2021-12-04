module AoCFile

let GetInput day = Seq.cache (System.IO.File.ReadLines $"Input/day{day}.txt")
let GetTestInput day = Seq.cache (System.IO.File.ReadLines $"Input/day{day}_test.txt")