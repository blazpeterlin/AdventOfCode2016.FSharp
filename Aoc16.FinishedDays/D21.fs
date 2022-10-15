module Aoc16.D21

open Aoc16
open Input
open TextCopy
open System.Collections.Generic
open Aoc16.Operators
open Microsoft.FSharp.Core.Operators.Checked

let input =  "input.txt" |> f2text

type DIR = LEFT|RIGHT
type CMD = 
    | SWAPIDX of int*int
    | SWAPLTR of char*char
    | ROTATE of DIR*int
    | ROTATEPOS of char
    | REVERSE of int*int
    | MOVE of int*int

let parseLine (ln:string) =
    ln
    |> text2tokens " "
    |> fun xs -> 
        match xs with
        | "swap"::"position"::x::"with"::"position"::y::[] -> SWAPIDX(int x, int y)
        | "swap"::"letter"::x::"with"::"letter"::y::[] -> SWAPLTR(x[0],y[0])
        | "rotate"::dir::x::_::[] -> ROTATE((match dir with |"left"->LEFT|"right"->RIGHT),int x)
        | "rotate"::"based"::"on"::"position"::"of"::"letter"::x::[] -> ROTATEPOS(x[0])
        | "reverse"::"positions"::x::"through"::y::[] -> REVERSE(int x, int y)
        | "move"::"position"::x::"to"::"position"::y::[] -> MOVE(int x, int y)
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let rec step (str:string) (cmd:CMD) =
    match cmd with
    | SWAPIDX(x,y) -> 
        let ch1 = str[x]
        let ch2 = str[y]
        step str (SWAPLTR(ch1,ch2))
    | SWAPLTR(a,b) -> 
        str.Replace(a,'ć').Replace(b,a).Replace('ć',b)
    | ROTATE(dir, steps) ->
        let realSteps = steps % str.Length
        match dir with
        | RIGHT -> str.Substring(str.Length-realSteps) + str.Substring(0, str.Length-realSteps)
        | LEFT -> str.Substring(realSteps) + str.Substring(0, realSteps)
    | ROTATEPOS(ch) ->
        let idx = str.IndexOf ch
        step str (ROTATE(RIGHT,idx+1+(if idx>=4 then 1 else 0)))
    | REVERSE(x,y) ->
        let a = min x y
        let b = max x y
        str.Substring(0, a) + (str.Substring(a,b-a+1).ToCharArray() |> Array.rev |> System.String) + str.Substring(b+1)
    | MOVE(x,y) ->
        let str1 = str.Substring(0,x)+str.Substring(x+1)
        let str2 = str1.Substring(0,y)+str[x].ToString()+str1.Substring(y)
        str2


let rec unstep (str:string) (cmd:CMD) =
    let r = 
        match cmd with
        | SWAPIDX(x,y) -> 
            let ch1 = str[x]
            let ch2 = str[y]
            unstep str (SWAPLTR(ch1,ch2))
        | SWAPLTR(a,b) -> 
            str.Replace(a,'ć').Replace(b,a).Replace('ć',b)
        | ROTATE(dir, steps) ->
            let realSteps = steps % str.Length
            match dir with
            | LEFT -> str.Substring(str.Length-realSteps) + str.Substring(0, str.Length-realSteps)
            | RIGHT -> str.Substring(realSteps) + str.Substring(0, realSteps)
        | ROTATEPOS(ch) ->
            let idx = str.IndexOf ch
            let oldIdx = [0..str.Length-1] |> List.find (fun i -> (i+i+1+(if i>=4 then 1 else 0))%str.Length = idx)
            unstep str (ROTATE(RIGHT,oldIdx+1+(if oldIdx>=4 then 1 else 0)))
        | REVERSE(x,y) ->
            let a = min x y
            let b = max x y
            str.Substring(0, a) + (str.Substring(a,b-a+1).ToCharArray() |> Array.rev |> System.String) + str.Substring(b+1)
        | MOVE(y,x) ->
            let str1 = str.Substring(0,x)+str.Substring(x+1)
            let str2 = str1.Substring(0,y)+str[x].ToString()+str1.Substring(y)
            str2
    if (step r cmd)<>str then failwith "oops" else
    r

let solve1 (text:string) = 
    let inp = text |> parse2lines

    let plaintext = "abcdefgh"

    inp
    |> List.fold step plaintext
    
let solve2 (text:string) =
    let inp = text |> parse2lines

    let plaintext = "fbgdceah"

    inp
    |> List.rev
    |> List.fold unstep plaintext

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()