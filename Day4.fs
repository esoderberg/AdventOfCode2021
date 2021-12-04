module Day4
open AoCFile

module Bingo =
    type BingoBoard = 
        {
            Size : int
            Won : bool
            NumberPos : Map<int, int*int>
            BoardPos : Map<int*int, bool>
        }
        // Gets the position of the number
        member this.GetPosOf number = Map.tryFind number this.NumberPos
        // Mark the position 
        member this.Mark position = {this with BoardPos = this.BoardPos.Add(position, true); Won = (this.CheckWin position) }
        // See if position is marked, returns false for OOB positions
        member this.IsMarked position = 
            match Map.tryFind position this.BoardPos with
            | Some(mark) -> mark
            | None -> false
            
        member this.WinningRow row = Seq.forall (fun b -> b) (seq { for col in 0..this.Size-1 do yield this.BoardPos.[(row,col)]})
        member this.WinningColumn col = Seq.forall (fun b -> b) (seq { for row in 0..this.Size-1 do yield this.BoardPos.[(row,col)]})
        member this.WinningDiagonal forwardDiagonal = 
            Seq.forall (fun b -> b) 
                (seq { 
                    for pos in 0..this.Size-1 do                     
                        if forwardDiagonal then
                            yield this.BoardPos.[(pos,pos)] // 0,0 1,1 2,2
                        else
                            yield this.BoardPos.[(pos, this.Size-1-pos)] // 0,4 1,3 2,2 3,1 4,0
                })
        // Check for victory extending from pos
        member this.CheckWin pos = 
            let (row, col) = pos
            (this.WinningRow row) 
            || (this.WinningColumn col) 
            || if(row+col = this.Size-1 || row=col) then ((this.WinningDiagonal true) || (this.WinningDiagonal false)) else false
            
    let CreateBoard (rows: seq<int>) = 
        let size = int(sqrt (float (Seq.length rows)))
        {
        Size = size;
        Won = false; 
        NumberPos = Map (Seq.mapi (fun idx elem -> (elem, (idx % size, idx / size)) ) rows);
        BoardPos =  Map (seq { for i in 0..size-1 do for j in 0..size-1 do yield ((j,i), false) })
        }

    let Play number (board:BingoBoard) = 
        match board.GetPosOf number with
        | Some(pos) -> board.Mark pos
        | None -> board
        

let rec BuildBingoBoards input size =
    if List.isEmpty input then 
        List.empty
    else 
        let boardStrings = (List.take size (List.skip 1 input))
        let boardNumbers = 
            List.collect (fun (s: string) -> List.ofArray (s.Split(' ', System.StringSplitOptions.RemoveEmptyEntries))) boardStrings
            |> List.map (fun s -> int s)  
        
        (Bingo.CreateBoard boardNumbers) :: (BuildBingoBoards (List.skip (size+1) input) size)

let ParseInput (input:list<string>) = 
    let numbers = (List.map (fun s -> int s) (List.ofArray ((List.head input).Split(','))))
    let boards = BuildBingoBoards(List.skip 1 input) 5
    (numbers, boards)

let PlayBingoRound number boards = (List.map (Bingo.Play number) boards)

// Play bingo and return the first winning board
let rec PlayBingo numbers boards = 
    let updatedBoards = PlayBingoRound (List.head numbers) boards
    let wonBoards = List.filter (fun (board: Bingo.BingoBoard) -> board.Won) updatedBoards
    if wonBoards.Length > 0 then (List.head numbers, wonBoards)
    else PlayBingo (List.tail numbers) updatedBoards

    

let ExecutePart1 input = 
    let (numbers, boards) = ParseInput input
    let (winningNumber, winningBoards) = PlayBingo numbers boards
    
    

let Execute (withFileInput:bool) = 
    let input = if withFileInput then List.ofSeq (GetInput 4) else List.ofSeq (GetTestInput 4)
    ExecutePart1 input
    0