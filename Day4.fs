module Day4
open AoCFile

module Bingo =
    type Position = { row:int; col:int; }
    type BoardCell = { number:int; marked:bool }
    type BingoBoard = 
        {
            Size : int
            Won : bool
            // Number mapped to its board position
            NumberPos : Map<int, Position>
            // Position mapped to the number and if the position is marked
            BoardPos : Map<Position, BoardCell>
        }
        // Gets the position of the number
        member this.GetPosOf number = Map.tryFind number this.NumberPos
        member this.GetCell pos = this.BoardPos.[pos]
        member this.NumberOf pos = this.BoardPos.[pos].number
        // Mark the position 
        member this.Mark position = 
            let updatedBoard = {this with BoardPos = this.BoardPos.Add(position, {this.GetCell position with marked=true})}
            {updatedBoard with Won = updatedBoard.CheckWin position}

        // See if position is marked, returns false for OOB positions
        member this.IsMarked position = 
            match Map.tryFind position this.BoardPos with
            | Some(cell) -> cell.marked
            | None -> false
            
        member this.WinningRow row = Seq.forall (fun cell -> cell.marked) (seq { for col in 0..this.Size-1 do yield this.BoardPos.[{row=row; col=col}]})
        member this.WinningColumn col = Seq.forall (fun cell -> cell.marked) (seq { for row in 0..this.Size-1 do yield this.BoardPos.[{row=row;col=col}]})
        //member this.WinningDiagonal forwardDiagonal = 
        //    Seq.forall (fun b -> b) 
        //        (seq { 
        //            for pos in 0..this.Size-1 do                     
        //                if forwardDiagonal then
        //                    yield this.IsMarked {row=pos;col=pos} // 0,0 1,1 2,2
        //                else
        //                    yield this.IsMarked {row=pos;col=this.Size-1-pos} // 0,4 1,3 2,2 3,1 4,0
        //        })
        // Check for victory extending from pos
        member this.CheckWin pos = (this.WinningRow pos.row) || (this.WinningColumn pos.col) 
            // || if(pos.row+pos.col = this.Size-1 || pos.row=pos.col) then ((this.WinningDiagonal true) || (this.WinningDiagonal false)) else false
            
    let Print (board: BingoBoard) = 
        let positions = [for i in 0..board.Size-1 -> [for j in 0..board.Size-1 -> {row=i;col=j}]]
        let rowStringifier = (fun pos -> if board.IsMarked pos then $"""{$"({board.NumberOf pos})",4}""" else $"{board.NumberOf pos,4}")
        let stringRows = List.map (fun row -> String.concat " " (List.map rowStringifier row)) positions
        List.iter (fun row -> printfn "%s" row) stringRows
        printfn ""
        
    let CreateBoard (rows: int list) = 
        let size = int(sqrt (float (Seq.length rows)))
        let positions = [for i in 0..size-1 do for j in 0..size-1 -> {row=i;col=j}]
        {
        Size = size;
        Won = false; 
        NumberPos = Map (List.map2 (fun num pos -> (num,pos)) rows positions)
        BoardPos =  Map ( List.map2 (fun pos (num:int) -> (pos,{number=num; marked=false})) positions rows)
        }
        

    let Play number (board:BingoBoard) = 
        match board.GetPosOf number with
        | Some(pos) -> 
            let nb = board.Mark pos
            Print nb
            nb
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
    
    let drawnNumber = List.head numbers
    printfn "Drew number %d" drawnNumber
    let updatedBoards = PlayBingoRound drawnNumber boards
    let wonBoards = List.filter (fun (board: Bingo.BingoBoard) -> board.Won) updatedBoards
    if wonBoards.Length > 0 then (drawnNumber, wonBoards)
    else
        printfn "-------------------------------------------"
        PlayBingo (List.tail numbers) updatedBoards

    

let ExecutePart1 input = 
    let (numbers, boards) = ParseInput input
    let (winningNumber, winningBoards) = PlayBingo numbers boards
    let winningBoard = List.head winningBoards
    let sumUnmarked = Seq.sumBy (fun (k, (v: Bingo.BoardCell)) -> if not v.marked then v.number else 0) (Map.toSeq winningBoard.BoardPos)
    printfn "Unmarked sum: %d" sumUnmarked
    printfn "Winning number %d" winningNumber
    printfn "Day 4, part 1: %d" (sumUnmarked * winningNumber)

    
    

let Execute (withFileInput:bool) = 
    let input = if withFileInput then List.ofSeq (GetInput 4) else List.ofSeq (GetTestInput 4)
    ExecutePart1 input
    0