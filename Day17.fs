module Day17

open AoCFile
open System

// Calculates the velocity range that will stop the probe at the right x-coordinates
let CalculateXVelocityRange targetLeft targetRight =
    let lowestVelocity = int (Math.Ceiling (-1.0/2.0 + sqrt (1.0/4.0 + 2.0* float targetLeft)))
    let highestVelocity = int (Math.Floor  (-1.0/2.0 + sqrt (1.0/4.0 + 2.0* float targetRight)))
    (lowestVelocity, highestVelocity)

// Calculate the speed neded to hit within bottom or top at step 'step'
// Use the fact that the first arc always hits the 0 position on the way down.
// At that zero position the velocity will be -initialvelocity-1
// So to max speed we want to go from y=0 to y=bottom in one step
let CalculateYSpeedForMaxY bottom top = -(bottom + 1)

let ExecutePart1 ((left,right),(bottom,top)) = 
    let (lhv, hhv) = CalculateXVelocityRange left right
    let ySpeed = CalculateYSpeedForMaxY bottom top 
    printfn "Day 17, Part 1: %d" (int (float ((ySpeed*ySpeed)+ySpeed)/2.0))

let Execute withRealInput = 
    //If left and right had been negative we could have just pretended that the negative direction was positive.
    let ((left,right),(bottom,top)) = if withRealInput then ((153,199),(-114,-75)) else ((20,30),(-10,-5))
    ExecutePart1 ((left,right),(bottom,top))
    
    ()