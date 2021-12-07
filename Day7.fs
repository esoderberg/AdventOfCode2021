module Day7
open AoCFile

let TestInput = seq {"16,1,2,0,4,2,7,1,2,14"}


let GetMiddlePosition (sortedInput: int[]) = 
    let halfLength = sortedInput.Length/2
    if (sortedInput.Length % 2) = 0 then 
       sortedInput.[halfLength]
    else
       (sortedInput.[halfLength] + sortedInput.[halfLength+1])/2

let CalculateFuelExpenditure (fuelFunc:int->int) positions = 
    let fuelExpenditures = Array.map fuelFunc positions
    let totalfuelExpenditure = Array.sum fuelExpenditures
    totalfuelExpenditure

// Solving part 1 just requires us to take the median position
// The median position is nearest everyone
let ExecutePart1 input = 
    let sortedInput = Array.sort input
    let middle = GetMiddlePosition sortedInput
    let fuelSpent = CalculateFuelExpenditure (fun pos -> abs (middle - pos)) sortedInput

    printfn "Day 7, Part1: Fuel spent: %d, Middle: %d" fuelSpent middle
    
let part2FuelFunction dest pos = 
    let dist = abs(dest - pos)
    (dist+dist*dist)/2

// Calculates a list of expenditures to reach every position 
// Essentially we calculate a matrix where the sum of row N is the crabs fuel expenditure to reach position N
// Then we just select the row which has the smallest fuel expenditure. 
// The solution is O(D*N) where D is the number of positions between the first and last crab and N is the number of crabs.
let ExecutePart2 input = 
    let sortedInput = Array.sort input
    let expenditures = [for i in sortedInput.[0]..sortedInput.[sortedInput.Length-1] -> (i, CalculateFuelExpenditure (part2FuelFunction i) sortedInput)]
    let minimizedExpenditure = List.minBy (fun (pos, fuel) -> fuel) expenditures
    printfn "Day 7, Part2: Middle: %d , Fuel spent: %d" (fst minimizedExpenditure) (snd minimizedExpenditure)

let Execute withRealInput = 
    let input = Seq.toArray (Seq.map int ((Seq.head (if withRealInput then GetInput 7 else TestInput)).Split ","))
    ExecutePart1 input
    ExecutePart2 input