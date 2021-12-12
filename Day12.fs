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

type CaveNetwork(nodes:Set<string>, edges:Set<string*string>) = 
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

    member val Neighbours = neighbours

// Find the paths in the cave network, built recursively
let rec FindPathsRec (caveNetwork:CaveNetwork) (currentNode:string) (endNode:string) (visited:Set<string>) = 
    if currentNode = endNode then [[currentNode]]
    else
        let visited = if IsSmall currentNode then visited.Add currentNode else visited
        let neighbours = 
            caveNetwork.Neighbours.[currentNode]
            |> Set.toList
            |> List.filter (fun cave -> not (Set.contains cave visited))
        let paths = List.collect (fun neighbour -> List.map (fun s -> currentNode::s) (FindPathsRec caveNetwork neighbour endNode visited)) neighbours
        paths

let FindPaths caveNetwork startNode endNode = FindPathsRec caveNetwork startNode endNode (set [])
    

let Parse (line:string) = 
    let parts = line.Split '-'
    let (n1, n2) = (parts.[0], parts.[1])
    // Return Nodes & Edges
    (set [n1;n2], set [(n1,n2);(n2,n1)])



let ExecutePart1 caveNetwork =
    let paths =  FindPaths caveNetwork "start" "end"
    printfn "Day 12, Part 1: %d" (List.length paths)

let Execute withRealInput = 
    let lines = if withRealInput then GetInput 12 else GetTestInput 12
    let parsed = Seq.map Parse lines
    let (nodes, edges) = Seq.fold (fun (nodes, edges) (pnodes, pedges) -> ((nodes+pnodes),(edges+pedges))) ((set []),(set [])) parsed
    let caveNetwork = CaveNetwork(nodes,edges)
    ExecutePart1 caveNetwork