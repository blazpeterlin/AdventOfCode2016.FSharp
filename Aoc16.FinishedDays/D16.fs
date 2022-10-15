module Aoc16.D16

open Aoc16
open Input
open TextCopy
open System.Collections.Generic
open Aoc16.Operators
open Microsoft.FSharp.Core.Operators.Checked

let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    ln
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine
    |>List.head

let step x =
    x + "0" + (x.ToCharArray() |> Array.rev |> Array.map (function | '1' -> '0' | '0' -> '1') |> System.String)

let fillDisk diskLen x0 =
    x0
    |> Seq.unfold (fun str -> 
        let str2 = step str
        Some(str2,str2))
    |> Seq.find (fun x -> x.Length >= diskLen)
    |> fun x -> x.Substring(0, diskLen)

let rec checkSum (disk:char seq) =
    disk
    |> Seq.chunkBySize 2
    |> Seq.map (fun xs -> if xs[0]=xs[1] then 1 else 0)
    |> fun xs -> xs|>Seq.map (fun i -> i.ToString()) |> Seq.concat
    |> Seq.cache
    |> fun resultStr ->
        if (Seq.length resultStr)%2=1
        then resultStr
        else checkSum resultStr

let solve1 (text:string) = 
    let inp = text |> parse2lines
    let diskLen = 272
    
    let disk = fillDisk diskLen inp |> fun s -> s.ToCharArray() |> Seq.ofArray

    checkSum disk |> Array.ofSeq |> System.String
    
let solve2 (text:string) =
    let inp = text |> parse2lines
    let diskLen = 35651584
    
    let disk = fillDisk diskLen inp |> fun s -> s.ToCharArray() |> Seq.ofArray

    checkSum disk |> Array.ofSeq |> System.String

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()