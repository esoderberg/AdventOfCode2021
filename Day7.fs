module Day7
open AoCFile

let TestInput = seq {"16,1,2,0,4,2,7,1,2,14"}


let GetMiddlePosition (sortedInput: int[]) = 
    let halfLength = sortedInput.Length/2
    if (sortedInput.Length % 2) = 0 then 
       sortedInput.[halfLength]
    else
       (sortedInput.[halfLength] + sortedInput.[halfLength+1])/2

let CalculateFuelSpentTo destination positions = 
    let fuelExpenditures = Array.map (fun pos -> abs (destination - pos)) positions
    let totalfuelExpenditure = Array.sum fuelExpenditures
    totalfuelExpenditure

let ExecutePart1 input = 
    let sortedInput = Array.sort input
    let middle = GetMiddlePosition sortedInput
    let fuelSpent = CalculateFuelSpentTo middle sortedInput

    printfn "Day 7, Part1: Fuel spent: %d, Middle: %d" fuelSpent middle
    


let Execute withRealInput = 
    let input = Seq.toArray (Seq.map int ((Seq.head (if withRealInput then GetInput 7 else TestInput)).Split ","))
    ExecutePart1 input