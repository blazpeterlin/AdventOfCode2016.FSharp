module Aoc16.D03

open Aoc16
open Input
open TextCopy
open System.Collections.Generic
open Aoc16.Operators

let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    ln
    |> text2tokens " "
    |> map int
    |> List.sort
    // |> text2tokensStr ["abc";"def"]
    
let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let parseChunk (lns:string list) =
    lns
    |> List.map (fun ln -> ln |> text2tokens " " |> map int)
    |> List.transpose
    |> List.sort

let parse2lines2 (text:string) = 
    text
    |> text2lines 
    |> List.chunkBySize 3
    |> List.map (parseChunk)
    |> List.concat
    |> List.map (List.sort)

let solve1 (text:string) = 
    text |> parse2lines |> filter (fun xs -> xs[0] + xs[1] > xs[2]) |> length
    
    
let solve2 (text:string) =
    text |> parse2lines2 |> filter (fun xs -> xs[0] + xs[1] > xs[2]) |> length
    

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()