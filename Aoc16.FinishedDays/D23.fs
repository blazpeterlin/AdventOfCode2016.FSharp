module Aoc16.D23

open Aoc16
open Input
open TextCopy
open System.Collections.Generic
open Aoc16.Operators
open Microsoft.FSharp.Core.Operators.Checked

let input =  "input.txt" |> f2text

//type RegOrVal =
//    | Reg of char
//    | Val of int

//type Instruction =
//    | CPY of RegOrVal * RegOrVal
//    | INC of char
//    | DEC of char
//    | JNZ of RegOrVal * RegOrVal
//    | TGL of RegOrVal
//    | NIL

//type ProgramState = { Regs : Map<char,int>; Instructions: Map<int,Instruction>; Pointer:int; }

//let toggleInstruction (ins : Instruction) = 
//    match ins with
//    | INC(reg) -> DEC(reg)
//    | DEC(reg) -> INC(reg)
//    | TGL(Reg r) -> INC(r)
//    | TGL(Val v) -> NIL
//    | JNZ(a, b) -> CPY(a,b)
//    | CPY(a, b) -> JNZ(a,b)
//    | NIL -> NIL

//let toValue (regs:Map<char,int>) regOrVal = 
//    match regOrVal with | Val v -> v | Reg r -> regs[r]
    

//let step (ps:ProgramState) =
//    if (ps.Instructions.ContainsKey ps.Pointer |> not) then None else
//    let ins = ps.Instructions[ps.Pointer]
//    let regs = ps.Regs
//    let ps2 = 
//        match ins with
//        | CPY(Val v1, Reg reg) -> { Regs = regs.Add(reg,v1); Instructions = ps.Instructions; Pointer = ps.Pointer+1 }
//        | CPY(Reg r1, Reg reg) -> { Regs = regs.Add(reg,regs[r1]); Instructions = ps.Instructions; Pointer = ps.Pointer+1 }
//        | INC(reg) -> { Regs = ps.Regs.Add(reg,regs[reg]+1); Instructions = ps.Instructions; Pointer = ps.Pointer+1 }
//        | DEC(reg) -> { Regs = ps.Regs.Add(reg,regs[reg]+1); Instructions = ps.Instructions; Pointer = ps.Pointer+1 }
//        | JNZ(Val v1, regOrVal) ->  
//            let offset = regOrVal |> toValue regs
//            { Regs=ps.Regs; Instructions = ps.Instructions; Pointer = if v1<>0 then ps.Pointer+offset else ps.Pointer+1 }
//        | JNZ(Reg r1, regOrVal) -> 
//            let offset = regOrVal |> toValue regs
//            { Regs=ps.Regs; Instructions = ps.Instructions; Pointer = if regs[r1]<>0 then ps.Pointer+offset else ps.Pointer+1 }
//        | TGL(regOrVal) -> 
//            let offset = regOrVal |> toValue regs
//            if   (*(ps.Instructions.ContainsKey(ps.Pointer+offset) |> not) || *)offset=0
//            then { ps with Pointer = ps.Pointer+1 }
//            else { Regs = ps.Regs; Instructions = ps.Instructions.Add(ps.Pointer+offset,(toggleInstruction ps.Instructions[ps.Pointer+offset])); Pointer = ps.Pointer+1 }
//        | _ -> { ps with Pointer = ps.Pointer+1 }
//    Some ps2

//let toRegOrVal (str:string) =
//    match System.Int32.TryParse str with | true, xi -> Val(xi) | false, _ -> Reg(str[0])

//let parseLine (ln:string) =
//    ln
//    |> text2tokens " "
//    |> fun xs -> 
//        match xs with
//        | "cpy"::x::y::[] -> CPY( toRegOrVal x  , Reg(y[0]))
//        | "inc"::x::[] -> INC(x[0])
//        | "dec"::x::[] -> DEC(x[0])
//        | "jnz"::x::y::[] -> JNZ( toRegOrVal x  , toRegOrVal y )
//        | "tgl"::x::[] -> TGL( toRegOrVal x )

//let parse2lines (text:string) = 
//    text
//    |> text2lines 
//    |> List.map parseLine

let solve1 (text:string) = 
    //let inp = text |> parse2lines
    //let ps0 = { Instructions = inp |> List.indexed |> Map; Regs = ['a',7;'b',0;'c',0;'d',0]|>Map; Pointer=0; }

    //ps0
    //|> Seq.unfold (fun ps ->
    //    let psNext = step ps
    //    match psNext with
    //    | None ->None
    //    | Some psn -> Some(psn,psn)
    //)
    //|> Seq.last
    //|> fun ps -> ps.Regs['a']
    



    // decompiled by hand, the "proper" automated solution would never finish

    let mutable a = 7
    let mutable b = 6
    let mutable d = 0
    let mutable c = 0

    while c<>2 do
        d <- a
        a <- 0

        while d <> 0 do
            c <- b
            while c <> 0 do
                a <- a+1
                c <- c-1
            d <- d-1
        b<- b-1
        c <- b
        d<- c
        while d <> 0 do
            d <- d-1
            c <- c+1

    c <- -16
    c <- 1
    c <- 75
    while (c <> 0) do
        d <- 88
        while (d <> 0) do
            a<- a+1
            d<- d-1
        c <- c-1

    a


    
let solve2 (text:string) =
    // decompiled by hand, the "proper" automated solution would never finish
    let mutable a = 12
    let mutable b = 11
    let mutable d = 0
    let mutable c = 0

    while c<>2 do
        d <- a
        a <- 0

        while d <> 0 do
            c <- b
            while c <> 0 do
                a <- a+1
                c <- c-1
            d <- d-1
        b<- b-1
        c <- b
        d<- c
        while d <> 0 do
            d <- d-1
            c <- c+1

    c <- -16
    c <- 1
    c <- 75
    while (c <> 0) do
        d <- 88
        while (d <> 0) do
            a<- a+1
            d<- d-1
        c <- c-1

    a

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res1.ToString())

let finished = true
()