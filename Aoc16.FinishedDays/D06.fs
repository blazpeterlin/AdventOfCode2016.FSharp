module Aoc16.D06

open Aoc16
open Input
open TextCopy
open System.Collections.Generic
open Aoc16.Operators

let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    ln.ToCharArray() |> ofArray |> map (fun x -> x.ToString())
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let solve1 (text:string) = 
    let res = 
        text 
        |> parse2lines 
        |> List.transpose 
        |> map (List.countBy id) 
        |> map (List.sortByDescending snd) 
        |> map List.head 
        |> map fst 
        |> List.reduce (+)
    res
    
let solve2 (text:string) =
    let res = 
        text 
        |> parse2lines 
        |> List.transpose 
        |> map (List.countBy id) 
        |> map (List.sortBy snd) 
        |> map List.head 
        |> map fst 
        |> List.reduce (+)
    res

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()