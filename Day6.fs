module Day6
open AoCFile

let TestInput = seq {"3,4,3,1,2"}

let BornFish = 8
let NewMotherFish = 6

// Simulate the fish for a day and either return itself one day closer to birth
// or the reset self together with the newborn fish
let SimulateFishDay (fish: int) = 
    if fish = 0 then // Fish is giving birth today
        [NewMotherFish; BornFish]
    else
        [fish-1]

// Simulates one day for all fishes
let SimulateFishesDay (fishes: int list) = List.collect SimulateFishDay fishes

// Simulates 'maxDays' number of days.
// The result is returned in an array where each index contains the fish list for that day.
let rec SimulateDaysR currentDay maxDays (fishes: int list) (result: int list[]) =
    (result.[currentDay] <- fishes)
    if currentDay = maxDays then result
    else 
        printfn "Now simulating day %d" (currentDay+1)
        SimulateDaysR (currentDay+1) maxDays (SimulateFishesDay fishes) result

// Simulates the fishes for the given number of days
let SimulateDays days (fishes: int list) = SimulateDaysR 0 days fishes (Array.create (days+1) List.empty<int>)


let ExecutePart1 days (fishes: int list) shouldPrint= 
    let simulated = SimulateDays days fishes
    if shouldPrint then Array.iteri (fun idx l -> printfn "After %d days: %s" idx (String.concat "," (List.map string l))) simulated
    printfn "Day 6, Part 1: %d" (simulated.[simulated.Length-1].Length)

let rec SimulateFishesAggregated currentDay days (fishesByDaysToBirth:int64[]) shouldPrint =
    if currentDay = days then fishesByDaysToBirth
    else 
        let mothers = fishesByDaysToBirth.[0]
        let newborns = mothers
        for i in 0..7 do
            fishesByDaysToBirth.[i] <- fishesByDaysToBirth.[i+1]
        fishesByDaysToBirth.[8] <- newborns
        fishesByDaysToBirth.[6] <- fishesByDaysToBirth.[6] + mothers
        if shouldPrint then printfn "Day %d Fishes %d, Stages: %s" (currentDay+1) (Array.sum fishesByDaysToBirth) (String.concat "," (Array.map string fishesByDaysToBirth))
        SimulateFishesAggregated (currentDay+1) days fishesByDaysToBirth shouldPrint


let ExecutePartFast part days (fishes: int list) shouldPrint =
    let fishesByDaysToBirth = Array.create 9 0L
    let count = List.countBy id fishes
    List.iter (fun (days,fishCount) -> (fishesByDaysToBirth.[days] <- int64 fishCount)) count
    let result = SimulateFishesAggregated 0 days fishesByDaysToBirth shouldPrint
    printfn "Day 6, Part %d: %d" part (Array.sum result)


let Execute withFileInput = 
    let input = Seq.head (if withFileInput then GetInput 6 else TestInput)
    let fishes = List.ofSeq (Seq.map int (input.Split ','))
    ExecutePart1 80 fishes false 
    ExecutePartFast 1 80 fishes false
    ExecutePartFast 2 256 fishes false