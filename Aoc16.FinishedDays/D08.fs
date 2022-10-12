module Aoc16.D08

open Aoc16
open Input
open TextCopy
open System.Collections.Generic
open Aoc16.Operators
open System

let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    ln
    |> text2tokens " ="
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let modifyScreen w h scr ln =
    scr
    |> map (fun ((x,y),v) ->
        match ln with
        | "rect"::[axb] ->
            let a,b = axb |> text2tokens "x" |> map int |> fun xs -> xs[0],xs[1]
            if x<a && y<b then (x,y),true
            else (x,y),v
        | "rotate"::"row"::_::strRow::"by"::[strRot] ->
            let row=int strRow
            let rot=int strRot
            if y=row then
                let newX = (x+rot)%w
                (newX,y),v
            else (x,y),v
        | "rotate"::"column"::_::strCol::"by"::[strRot] ->
            let col=int strCol
            let rot=int strRot
            if x=col then
                let newY = (y+rot)%h
                (x,newY),v
            else (x,y),v
    )

let solve1 (text:string) = 
    let inps = text |> parse2lines

    let w=50
    let h=6

    let scr0 = [
        for y in [0 .. h-1] do   
            for x in [0 .. w-1] do
                yield ((x,y),false)
    ]

    let scrN = 
        inps
        |> fold (fun scr inp -> modifyScreen w h scr inp) scr0
    scrN |> map snd |> filter id |> length
    
    
let solve2 (text:string) =
    let inps = text |> parse2lines

    let w=50
    let h=6

    let scr0 = [
        for y in [0 .. h-1] do   
            for x in [0 .. w-1] do
                yield ((x,y),false)
    ]

    let scrN = 
        inps
        |> fold (fun scr inp -> modifyScreen w h scr inp) scr0

    let scrSorted = 
        scrN
        |> List.sortBy (fun ((x,y),_) -> y,x)

    for (x,y),v in scrSorted do
        if x=0 then Console.WriteLine() else ()|>ignore
        if v then Console.Write("█") else Console.Write(" ");

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()