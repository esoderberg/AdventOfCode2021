module Day17

open AoCFile
open System

// Calculate the speed neded to hit within bottom or top at step 'step'
// Use the fact that the first arc always hits the 0 position on the way down.
// At that zero position the velocity will be -initialvelocity-1
// So to max speed we want to go from y=0 to y=bottom in one step
let CalculateYVelocityForMaxY bottom top = -(bottom + 1)

// Calculate the step at which the probe is at position y and having started with the velocity v
// note: Ignores the negative sqrt so it can't solve for targets hit going up (but there is no such target in the input)
let CalculateStepForY (v:int) (y:int) = ((2.0*float v+1.0) + sqrt (float (4*v*v + 4*v + 1 - 8*y)))/2.0

// returns -1 if x is not reachable
let CalculateStepForX (u: int) (x:int) = 
    let maxDist = ((u*u)+u)/2
    if x > maxDist then -1.0
    else 
        ((2.0*float u+1.0) - sqrt (float (4*u*u + 4*u + 1 - 8*x)))/2.0
        
// Retrieves valid x velocities.
// Returned as (velocity, firstStep, lastStep)
// Where firstStep is the first step inside the target and lastStep is the last step inside the target
// if ball stops in the target area then lastStep is stepMax
let CalculateValidXVelocities (left, right) stepMax = 
    [for vel in 0..right do
        let stepL = int (Math.Ceiling (CalculateStepForX vel left))
        let stepR = int (Math.Floor (CalculateStepForX vel right))
        // Has a step after the left position, right is not reachable which means that 
        // The ball stops at the x-range
        if stepL > 0 && stepR = -1 then yield (vel, stepL, stepMax)
        elif stepL <= stepR  && stepL >= 0 then yield (vel, stepL, stepR)
        ]

// Retrieves valid y velocities
// Returned as (velocity, firstStep, lastStep)
// The probe is within the target during firstStep to lastStep
let CalculateValidYVelocities (bottom, top) = 
    [for vel in bottom..(-bottom) do
        let stepL = int (Math.Ceiling (CalculateStepForY vel top))
        let stepH = int (Math.Floor (CalculateStepForY vel bottom))
        if stepL <= stepH then 
            yield (vel, stepL, stepH)
    ]

let FlattenVelocityWithSteps l = List.collect (fun (vel, sl, sr) -> [for i in sl..sr -> (vel,i)]) l

let ExecutePart1 ((left,right),(bottom,top)) = 
    let ySpeed = CalculateYVelocityForMaxY bottom top 
    printfn "Day 17, Part 1: %d" (int (float ((ySpeed*ySpeed)+ySpeed)/2.0))

let combine2 xs ys = [
    for x in xs do
    for y in ys do
    yield x, y ]

let ExecutePart2 ((left,right),(bottom,top)) = 

    let validY = CalculateValidYVelocities (bottom, top)
    
    let flattenedY = FlattenVelocityWithSteps validY
    let groupY = List.groupBy (fun (v, step) -> step) flattenedY
    // Step -> velocities list
    let mapY = Map (List.map (fun (step,vl) -> (step, List.map fst vl)) groupY)
    let maxStep = List.max (List.map fst groupY)
    let validX = CalculateValidXVelocities (left, right) maxStep
    let flattenedX = FlattenVelocityWithSteps validX
    let groupX = List.groupBy (fun (v, step) -> step) flattenedX
    // Step -> velocities list
    let mapX = Map  (List.map (fun (step,vl) -> (step,List.map fst vl)) groupX)
    let velocities = List.distinct (List.concat
            [for step in 0..maxStep do 
                if mapY.ContainsKey step && mapX.ContainsKey step then
                    let yvels = mapY.Item step  
                    let xvels = mapX.Item step
                    combine2 xvels yvels
            ])
    printfn ("Day 17, Part 2: %d") (List.length velocities)
    ()
    
let Execute withRealInput = 
    //If left and right had been negative we could have just pretended that the negative direction was positive.
    let ((left,right),(bottom,top)) = if withRealInput then ((153,199),(-114,-75)) else ((20,30),(-10,-5))
    ExecutePart1 ((left,right),(bottom,top))
    ExecutePart2 ((left,right),(bottom,top))
    
    ()