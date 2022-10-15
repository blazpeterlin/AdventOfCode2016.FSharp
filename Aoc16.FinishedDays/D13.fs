module Aoc16.D13

open Aoc16
open Input
open TextCopy
open System.Collections.Generic
open Aoc16.Operators
open Microsoft.FSharp.Core.Operators.Checked
open System


let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    ln |> int
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

type BLOCK = WALL | TRAVERSED | EMPTY

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine
    |> List.head

let toBlock num (x,y) =
    let n = x*x + 3*x + 2*x*y + y + y*y + num
    let bitsStr = System.Convert.ToString(n,2).ToCharArray()
    let numOnes = bitsStr |> ofArray |> filter ((=)'1') |> length
    if numOnes%2=1 then WALL else EMPTY

let expand num (st: ((int*int)*BLOCK) list) =
    let maxX = st |> map fst |> map fst |> List.max |> (+)1
    let maxY = st |> map fst |> map snd |> List.max |> (+)1

    let row = [-1..maxX] |> map (fun x -> (x,maxY),toBlock num (x,maxY))
    let col = [-1..maxY-1] |> map (fun y -> (maxX,y),toBlock num (maxX,y))

    st |> List.append row |> List.append col

let step (st: ((int*int)*BLOCK) list) =
    let maxX = st |> map fst |> map fst |> List.max
    let maxY = st |> map fst |> map snd |> List.max

    let m = st |> Map.ofList

    // must be at least 1 or we failed
    //let foundEmpty = st |> List.find (fun ((x,y),v) -> v=EMPTY && x>=0 && y>=0 && x < maxX && y < maxY)
    

    let transformations = seq {
        for x in 0 .. maxX-1 do
            for y in 0 .. maxY-1 do
                
                let block = 
                    if m[x,y]=WALL then WALL else
                    if m[x,y]=TRAVERSED then TRAVERSED else
                    if m[x-1,y]=TRAVERSED then TRAVERSED else
                    if m[x+1,y]=TRAVERSED then TRAVERSED else
                    if m[x,y-1]=TRAVERSED then TRAVERSED else
                    if m[x,y+1]=TRAVERSED then TRAVERSED else
                    EMPTY
                (x,y),block
    }

    let m2 = transformations |> Seq.fold (fun (mp:Map<int*int,BLOCK>) t -> mp.Add t) m

    m2 |> Map.toList

let prepareSt0 text =
    let inp = text |> parse2lines 
    let st00 = [(-1,-1),(toBlock inp (-1,-1))] 
    let st0 = 
        // 50 expansions were a guess, it could've been more or less
        [0..50]
        |> List.fold (fun st i -> st |> (expand inp)) st00
        //|> List.fold (expand inp) |> (expand inp) |> (expand inp) 
        |> Map.ofList |> Map.add (1,1) TRAVERSED |> Map.toList
    st0

let solve1 (text:string) = 
    let st0 = prepareSt0 text

    let allSts =
        st0
        |>Seq.unfold (fun st -> 
            let r = st |> step
            Some(r,r)
        )

    let targetPos = 31,39

    let r =
        allSts
        |> Seq.findIndex (fun st -> 
            let m = st |> Map.ofList
            m.ContainsKey targetPos && m[targetPos]=TRAVERSED
        )
        |> (+)1

    r
    
let solve2 (text:string) =
    let st0 = prepareSt0 text

    let st50 =
        [1..50]
        |>Seq.fold (fun st i -> 
            let r = st |> step
            r
        ) st0

    st50 |> map snd |> filter ((=)TRAVERSED) |> length

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()