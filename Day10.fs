module Day10
open AoCFile


type Symbol = {sym:char;  pos:int}

let SyntaxScore chr = 
    match chr with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> 0

let AutoCompletionScore chr = 
    match chr with
    | ')' -> 1
    | ']' -> 2
    | '}' -> 3
    | '>' -> 4
    | _ -> 0

let GetMatching chr =
    match chr with
    | '(' -> ')' 
    | '['-> ']' 
    | '{' -> '}' 
    | '<'-> '>' 
    | _ -> raise (invalidArg "chr" "Invalid character")

let IsMatch l r = (GetMatching l) = r


let SymMatchT (l,r) = IsMatch l.sym r.sym
let SymMatch l r = IsMatch l.sym r.sym
let IsSyntaxError (l,r) = not (IsMatch l.sym r.sym)



let (|LeftSym|RightSym|Err|) sym = 
    match sym.sym with 
        | '<' | '(' | '[' | '{' -> LeftSym
        | '>' | ')' | ']' | '}' -> RightSym
        | _ -> Err


let rec ParseTR symbols stack matched = 
    if symbols = [] then (stack, List.rev matched)
    else
        let (sym, rest) = (List.head symbols, List.tail symbols)
        match sym with
        | LeftSym -> ParseTR rest (sym::stack) matched
        | RightSym -> ParseTR rest (List.tail stack) ((List.head stack, sym)::matched)
        | _ -> raise (invalidOp "Invalid symbol")


let Parse symbols = ParseTR symbols [] []

let SyntaxCheck symbolPairs =
    List.map (fun symbolPair -> (symbolPair, SymMatchT symbolPair)) symbolPairs

let ExecutePart1 inputLines =
    let parses = List.map Parse (List.ofSeq inputLines)
    let syntaxChecked = List.map (fun (s,m:(Symbol*Symbol) list) -> (s,SyntaxCheck m)) parses
    let corruptedFilter = (fun m -> List.filter (fun (_,valid) -> not valid) m)
    let corrupted = List.concat (List.where (fun l -> List.length l > 0) (List.map corruptedFilter (List.map snd syntaxChecked)))
    List.iter (fun ((l,r),err) -> printfn "Expected %c, but found %c instead" (GetMatching l.sym) r.sym) corrupted
    let sum = List.sumBy (fun ((l,r),err) -> SyntaxScore r.sym) corrupted
    printfn "Day 10, Part 1: %d" sum
    let incomplete = List.filter (fun (s,m) -> List.forall (fun (_,b) -> b) m ) syntaxChecked
    incomplete


let ScoreAutocompletions completions = 
    List.fold (fun score chr -> score*5L + int64 (AutoCompletionScore chr)) 0L completions

let AutoComplete (stack:Symbol list) =  List.map (fun sym -> GetMatching sym.sym) stack
    
let ExecutePart2 incompleteParses = 
    let autoCompletions = List.map (fun (stack,matched) ->  AutoComplete stack) incompleteParses
    let scores = List.map ScoreAutocompletions autoCompletions
    let scoresArr = (List.toArray (List.sort scores))
    List.iter (fun (comps: char list,score) -> printfn "%s = %d" (System.String (Array.ofList comps)) score) (List.sortBy (fun (_,s) -> s) (List.zip autoCompletions scores))
    
    printfn "Day 10, Part 2: %d" scoresArr.[scoresArr.Length/2]
    


let Execute withRealInput = 
    let inputLines = Seq.map (fun s -> List.mapi (fun i c -> {sym=c; pos=i}) (List.ofSeq s)) (if withRealInput then GetInput 10 else GetTestInput 10)
    let incomplete = ExecutePart1 inputLines
    ExecutePart2 incomplete
    0
    

