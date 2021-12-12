module Day12
open AoCFile

type BigCave = {id:string}
type SmallCave = {id:string}

type Cave = BigCave | SmallCave

// Returns the node in edge that isn't the 'node'
let GetOther node edge = if fst edge = node then snd edge else fst edge
// Returns true if node is in edge
let InEdge node edge = fst edge = node || snd edge = node

let IsSmall (cave:string) = not (List.exists (fun c -> System.Char.IsUpper c) (List.ofSeq cave))
let IsBig (cave:string) = not (List.exists (fun c -> System.Char.IsLower c) (List.ofSeq cave))

type CaveNetwork(nodes:Set<string>, edges:Set<string*string>, startNode:string, endNode:string) = 
    let nodes = nodes
    let nodeList = Set.toList nodes
    let edgeList = Set.toList edges
    let neighbours = 
            List.map (fun node -> (node, 
                (List.filter (InEdge node) edgeList)
                |> List.map (GetOther node)
                |> set )
            ) nodeList
            |> dict
    member val Start = startNode
    member val End = endNode
    member val Neighbours = neighbours

// Find the paths in the cave network, built recursively
let rec FindPathsRec (caveNetwork:CaveNetwork) (currentNode:string) (visited:Set<string>) = 
    if currentNode = caveNetwork.End then [[currentNode]]
    else
        let visited = if IsSmall currentNode then visited.Add currentNode else visited
        let neighbours = 
            caveNetwork.Neighbours.[currentNode]
            |> Set.toList
            |> List.filter (fun cave -> not (Set.contains cave visited))
        let paths = List.collect (fun neighbour -> List.map (fun s -> currentNode::s) (FindPathsRec caveNetwork neighbour visited)) neighbours
        paths

let FindPaths caveNetwork startNode endNode = FindPathsRec caveNetwork caveNetwork.Start (set [])
    

let Parse (line:string) = 
    let parts = line.Split '-'
    let (n1, n2) = (parts.[0], parts.[1])
    // Return Nodes & Edges
    (set [n1;n2], set [(n1,n2);(n2,n1)])



let ExecutePart1 caveNetwork =
    let paths =  FindPaths caveNetwork "start" "end"
    printfn "Day 12, Part 1: %d" (List.length paths)



// Find the paths in the cave network, built recursively
let rec FindPathsRec2 (caveNetwork:CaveNetwork) (currentNode:string) (visited:Set<string>) hasFreebie = 
    if currentNode = caveNetwork.End then [[currentNode]]
    else
        let visited = if IsSmall currentNode then visited.Add currentNode else visited
        let neighbours = 
            caveNetwork.Neighbours.[currentNode]
            |> Set.toList
            |> List.filter (fun cave -> not (Set.contains cave visited))
        let visitedNeighbours = 
                caveNetwork.Neighbours.[currentNode]
                |> Set.toList
                |> List.filter (fun cave -> (Set.contains cave visited && hasFreebie && cave <> caveNetwork.Start))

        let paths = List.collect (fun neighbour -> List.map (fun s -> currentNode::s) (FindPathsRec2 caveNetwork neighbour visited hasFreebie)) neighbours
        let freebiePaths = List.collect (fun neighbour -> List.map (fun s -> currentNode::s) (FindPathsRec2 caveNetwork neighbour visited false)) visitedNeighbours
        List.append paths freebiePaths
    
let FindPaths2 caveNetwork= FindPathsRec2 caveNetwork caveNetwork.Start (set []) true

let ExecutePart2 caveNetwork = 
    let paths =  FindPaths2 caveNetwork
    printfn "Day 12, Part 1: %d" (List.length paths)

let Execute withRealInput = 
    let lines = if withRealInput then GetInput 12 else GetTestInputN 12 3 // I really should improve on the input getting...
    let parsed = Seq.map Parse lines
    let (nodes, edges) = Seq.fold (fun (nodes, edges) (pnodes, pedges) -> ((nodes+pnodes),(edges+pedges))) ((set []),(set [])) parsed
    let caveNetwork = CaveNetwork(nodes,edges,"start","end")
    ExecutePart1 caveNetwork
    ExecutePart2 caveNetwork