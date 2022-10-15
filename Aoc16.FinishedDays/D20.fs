module Aoc16.D20

open Aoc16
open Input
open TextCopy
open System.Collections.Generic
open Aoc16.Operators
open Microsoft.FSharp.Core.Operators.Checked

let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    ln
    |> text2tokens "-"
    |> fun xs -> int64 xs[0], int64 xs[1]
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let solve1 (text:string) = 
    let inp = text |> parse2lines
    let possibleNums = 
        inp
        |> List.map snd
        |> List.map ((+)1L)
        |> List.append [0L]
        |> List.sort
    possibleNums
    |> List.find (fun x -> 
        inp
        |> List.tryFind (fun (rngFrom,rngTo) -> rngFrom <= x && rngTo >= x)
        |> (=)None
    )
    
let solve2 (text:string) =
    let inp = text |> parse2lines
    let possibleRangeStarts =
        inp
        |> List.map snd
        |> List.map ((+)1L)
        |> List.sort
    let acceptableRanges =
        possibleRangeStarts
        |> List.filter (fun x -> 
            inp
            |> List.tryFind (fun (rngFrom,rngTo) -> rngFrom <= x && rngTo >= x)
            |> (=)None
        )
    acceptableRanges
    |> List.except [4294967296L]
    |> List.map (fun x ->
        let next = 
            inp
            |> List.filter (fun (rngFrom,rngTo) -> rngFrom > x)
            |> List.sort
            |> List.head
            |> fst
        x,(next-1L)
    )
    |> List.map (fun (xFrom, xTo) -> xTo-xFrom+1L)
    |> List.sum

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())
let finished = true
()