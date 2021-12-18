﻿module Day16

open AoCFile
open System

type Bit = { Set:bool }
    with
    override this.ToString() = if this.Set then "1" else "0"

let (|Set|Unset|) (bit:Bit) = if bit.Set then Set else Unset

let BitFrom (char: char) = { Set = (char = '1') }
let BitsFrom (string: string) = List.map BitFrom (List.ofSeq string) 
let Eval (bits : Bit list) = List.fold (fun acc bit -> (acc <<< 1) + (if bit.Set then 1 else 0)) 0 bits



type PacketHeader = {Version: int; TypeId: int}
type Packet = OperatorPacket of OperatorPacket | LiteralValuePacket of LiteralValuePacket
and LiteralValuePacket = {Header:PacketHeader; Value:int }
and OperatorPacket = {Header: PacketHeader; LengthTypeId:Bit; Packets: Packet list }

let rec SumVersions packet = 
    match packet with
    | LiteralValuePacket p -> p.Header.Version
    | OperatorPacket p -> p.Header.Version + (List.sumBy SumVersions p.Packets)

let (|Literal|Operator|) typeId = if typeId = 4 then Literal else Operator

let BitsFromHex hexChar = 
    BitsFrom (
        match hexChar with
        | '0' -> "0000"
        | '1' -> "0001"
        | '2' -> "0010"
        | '3' -> "0011"
        | '4' -> "0100"
        | '5' -> "0101"
        | '6' -> "0110"
        | '7' -> "0111"
        | '8' -> "1000"
        | '9' -> "1001"
        | 'A' -> "1010"
        | 'B' -> "1011"
        | 'C' -> "1100"
        | 'D' -> "1101"
        | 'E' -> "1110"
        | 'F' -> "1111"
        | _ -> raise (invalidArg "hexChar" $"not a hexadecimal character")
    )

let BitsFromHexes hexChars = List.collect BitsFromHex (List.ofSeq hexChars)

let ParseHeader (bits: Bit list) = 
    let version, bits = List.splitAt 3 bits
    let type_, bits = List.splitAt 3 bits
    {Version = Convert.ToInt32(String.Concat version, 2); TypeId= Convert.ToInt32(String.Concat type_, 2)}, bits


let rec ParsePacket bits =
    let header, bits = ParseHeader bits
    let (packet: Packet), bits = 
        (match header.TypeId with
        | Literal -> ParseLiteralPacket header bits |> (fun (p,b) -> (LiteralValuePacket p, b))
        | Operator -> ParseOperatorPacket header bits  |> (fun (p,b) -> (OperatorPacket p, b))
        )
    (packet, bits)

and ParseOperatorPacket header (bits: Bit list) : (OperatorPacket * Bit list) = 
    let length, bits = List.splitAt 1 bits
    let packets, bits = 
        match length.[0] with
        | Set -> 
            let subPacketCount = Eval (List.take 11 bits) 
            let bits = List.skip 11 bits
            ParseNumPackets subPacketCount bits
        | Unset -> 
            let subPacketBits = Eval (List.take 15 bits)
            let bits, remaining = List.splitAt subPacketBits (List.skip 15 bits)
            (ParsePackets bits, remaining)

    ( {Header = header; LengthTypeId = length.[0]; Packets = packets}, bits)

and ParseLiteralPacket header (bits: Bit list) : (LiteralValuePacket * Bit list)  = 
    let rec ParseLiteral bits = 
        if not (List.head bits).Set then 
            let end_, bits = List.splitAt 5 bits
            let end_ = (List.skip 1 end_)
            (end_,bits)
        else
            let literal, bits = List.splitAt 5 bits
            let tail, remaining =  ParseLiteral bits
            ((List.skip 1 literal) @ tail, remaining)

    let value, bits = ParseLiteral bits
    ({Header=header; Value = Eval value}, bits)

and ParsePackets bits : Packet list = 
    if List.length bits = 0 then []
    else 
    let packet, bits = ParsePacket bits
    (packet::(ParsePackets bits) )
    
and ParseNumPackets count bits = 
    if count = 0 then ([],bits)
    else
    let packet, bits = ParsePacket bits
    let packets, bits = ParseNumPackets (count-1) bits
    (packet::(packets),bits)





let TestInput1 = "D2FE28"
let Test1Result = {Header={Version=6; TypeId=4}; Value=2021}

let TestInput2 = "38006F45291200"
let Test2Result = {
    Header={Version=1; TypeId=6};
    LengthTypeId= {Set=false}
    Packets = [
        LiteralValuePacket {Header={Version=6; TypeId=4}; Value=10};
        LiteralValuePacket {Header={Version=2;TypeId=4}; Value=20}
        ]}


let TestInput3 = "8A004A801A8002F478"
let TestInput4 = "620080001611562C8802118E34"
let TestInput5 = "C0015000016115A2E0802F182340"
let TestInput6 = "A0016C880162017C3686B18A3D4780"

let CheckVersionSum expected input = 
    let packet,_ = (ParsePacket (BitsFromHexes (List.ofSeq input)))
    if expected = SumVersions packet then "Pass" else "Fail"

let ExecuteTests = 
    let bits = BitsFromHexes TestInput1
    let packet, rest = ParsePacket bits
    match packet with 
        | LiteralValuePacket p -> printfn "Test 1: %s" (if p = Test1Result then "Pass" else "Fail")
        | _ -> printfn "Error on Test 1"
    
    let bits = BitsFromHexes TestInput2
    let packet, rest = ParsePacket bits
    match packet with 
    | OperatorPacket p -> 
        printfn "Test 2: V: %d, T: %d, Passed: %s" (p.Header.Version) (p.Header.TypeId) (if p = Test2Result then "Pass" else "Fail")
    | _ -> printfn "Error on Test 2"
    
    printfn "Test 3: %s" (CheckVersionSum 16 TestInput3)
    printfn "Test 4: %s" (CheckVersionSum 12 TestInput4)
    printfn "Test 5: %s" (CheckVersionSum 23 TestInput5)
    printfn "Test 6: %s" (CheckVersionSum 31 TestInput6)
    ()




let ExecutePart1 lines = 
    let bits = List.collect (fun line -> List.collect (fun c -> BitsFromHex c) (List.ofSeq line)) lines
    let packet, bits = ParsePacket bits
    printfn "Day 16, Part 1: %d" (SumVersions packet)
    ()

let Execute withRealInput = 
    if not withRealInput then ExecuteTests
    else
    let lines = List.ofSeq (GetInput 16)
    ExecutePart1 lines
    ()