module Aoc16.D01

open Aoc16
open Input
open TextCopy
open System.Collections.Generic
open Aoc16.Operators

let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    ln
    |> text2tokens ", "
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let step (str:string) ((px,py),(dx,dy)) =
    let ins = str[0]
    let steps = str.Substring(1) |> int
    let (dx2, dy2) = 
        match ins with
        | 'L' -> (-dy,dx)
        | 'R' -> (dy,-dx)

    let dpos = (px,py) +.. (dx2 * steps, dy2 * steps)
    dpos,(dx2,dy2)

let stepUnbelievablySlowly ((px,py),(dx,dy),strList : string list) =
    let str::tail = strList
    let ins = str[0]
    let steps = str.Substring(1) |> int
    let (dx2, dy2) = 
        match ins with
        | 'L' -> (-dy,dx)
        | 'R' -> (dy,-dx)
        | 'F' -> (dx,dy)

    let dpos = (px,py) +.. (dx2, dy2)
    let nextStrs = if steps=1 then tail else ("F"+(steps-1).ToString())::tail
    dpos,(dx2,dy2),nextStrs
         
let manhattan (x,y) = 
    abs(x) + abs(y)

let solve1 (text:string) = 
    let inps = text |> parse2lines |> List.head
    let st0 = (0,0),(0,1)
    let (posN, dirN) = inps |> fold (fun st inp -> step inp st) st0
    manhattan posN

    
    
let solve2 (text:string) =
    let inps = text |> parse2lines |> List.head
    let st0 = (0,0),(0,1),inps
    let res = 
        st0 
        |> Seq.unfold (fun (p,d,strs) -> if strs.Length=0 then None else ((p,d,strs) |> stepUnbelievablySlowly |> fun r -> Some(r,r)))
        |> Seq.map (fun (x,y,z) -> x)
        |> ofSeq
        |> List.scan (fun st lst -> lst::st) []
        |> List.pairwise
        |> List.takeWhile (fun (x,_) -> (List.distinct x).Length = x.Length)
        |> List.last
        |> snd
        |> Common.listGroupByKeyVal id id
        |> List.sortByDescending (fun (x,lst) -> lst.Length)
        |> List.head
        |> fst

    manhattan res
    
let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()