module Aoc16.D18

open Aoc16
open Input
open TextCopy
open System.Collections.Generic
open Aoc16.Operators
open Microsoft.FSharp.Core.Operators.Checked

let input =  "input.txt" |> f2text

type TILE = TRAP | EMPTY

let parseLine (ln:string) =
    ln.ToCharArray()
    |> List.ofArray
    |> map (function | '.' -> EMPTY | '^' -> TRAP)
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine
    |> List.head

let nextTile wnd =
    match wnd with
    | TRAP::TRAP::[EMPTY] -> TRAP
    | EMPTY::TRAP::[TRAP] ->TRAP
    | TRAP::EMPTY::[EMPTY] -> TRAP
    | EMPTY::EMPTY::[TRAP]-> TRAP
    | _ -> EMPTY

let next (st:TILE list) =
    let sourceSt = EMPTY::st@[EMPTY]
    sourceSt
    |> List.windowed 3
    |> map nextTile

let solve1 (text:string) = 
    let inp = text |> parse2lines
    let entireBox = 
        [2..40]
        |> List.scan (fun st _ ->
            next st
        ) inp

    entireBox 
    |> List.concat
    |> List.filter ((=)EMPTY)
    |> List.length
    
let solve2 (text:string) =
    let inp = text |> parse2lines
    let entireBox = 
        seq { 2..400000 }
        |> Seq.scan (fun st _ ->
            next st
        ) inp

    entireBox 
    |> Seq.concat
    |> Seq.filter ((=)EMPTY)
    |> Seq.length

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()