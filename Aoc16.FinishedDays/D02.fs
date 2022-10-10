module Aoc16.D02

open Aoc16
open Input
open TextCopy
open System.Collections.Generic
open Aoc16.Operators

let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    ln.ToCharArray()
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let step1 (x,y) dir = 
    match dir with
    | 'L' -> max (x-1) 0, y
    | 'R' -> min (x+1) 2, y
    | 'D' -> x, min (y+1) 2
    | 'U' -> x, max (y-1) 0


let step2 (x,y) dir = 
    let offsetX = 
        match y with 
        | 0 | 4 -> 0 
        | 3 | 1 -> 1 
        | 2 -> 2
    let offsetY = 
        match x with 
        | 0 | 4 -> 0 
        | 3 | 1 -> 1 
        | 2 -> 2
    let r = 
        match dir with
        | 'L' -> max (x-1) (2-offsetX), y
        | 'R' -> min (x+1) (2+offsetX), y
        | 'D' -> x, min (y+1) (2+offsetY)
        | 'U' -> x, max (y-1) (2-offsetY)
    r
let stepFullLine stepFun (x,y) (line:char[]) =
    line |> ofArray |> fold stepFun (x,y)

let pos2res1 (x,y) =
    match (x,y) with
    | 0,0 -> 1
    | 1,0 -> 2
    | 2,0 -> 3
    | 0,1 -> 4
    | 1,1 -> 5
    | 2,1 -> 6
    | 0,2 -> 7
    | 1,2 -> 8
    | 2,2 -> 9
    
let pos2res2 (x,y) =
    match (x,y) with
    | 2,0 -> '1'
    | 1,1 -> '2'
    | 2,1 -> '3'
    | 3,1 -> '4'
    | 0,2 -> '5'
    | 1,2 -> '6'
    | 2,2 -> '7'
    | 3,2 -> '8'
    | 4,2 -> '9'
    | 1,3 -> 'A'
    | 2,3 -> 'B'
    | 3,3 -> 'C'
    | 2,4 -> 'D'


let solve1 (text:string) = 
    let inp = text |> parse2lines
    let c0 = (1,1)
    let nums = inp |> List.scan (stepFullLine step1) c0 |> map pos2res1
    let res = nums |> List.skip 1 |> fold (fun st num -> st*10+num) 0
    res
    
let solve2 (text:string) =
    let inp = text |> parse2lines
    let c0 = (0,2)
    let chs = inp |> List.scan (stepFullLine step2) c0 |> map pos2res2
    let res = chs |> List.skip 1 |> fold (fun st ch -> st+ch.ToString()) ""
    res

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()