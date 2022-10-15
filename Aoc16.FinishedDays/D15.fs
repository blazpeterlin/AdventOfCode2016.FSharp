module Aoc16.D15

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
    //Disc #1 has 13 positions; at time=0, it is at position 10.
    |> text2tokensStr ["Disc #";" has ";" positions; at time=0, it is at position ";"."]
    |> fun xs -> int xs[0],(int xs[1], int xs[2])

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let step (m:Map<int,int*int>) =     
    m
    |> Map.toSeq
    |> Seq.map (fun (key,(maxN,n)) ->
        key, (maxN, (n+1)%maxN)
    )
    |> Map.ofSeq

let tryTraverse (ms:Map<int,int*int> array) init =
    let m0 = ms[0]
    let max = m0.Keys |>Seq.max
    [1 .. max]
    |> Seq.fold (fun (success,mp) i -> 
        let nextM = ms[i]
        let r = ((snd nextM[i]) = 0)
        success && r, nextM
    ) (true,m0)
    |> fst

let solve1 (text:string) = 
    let st00 = text |> parse2lines |> Map.ofList
    let numFall = st00.Keys.Count
    
    Seq.initInfinite id
    |> Seq.scan (fun st _ -> step st) st00
    |> Seq.windowed (numFall+1)
    |> Seq.indexed
    |> Seq.find (fun (idx,sts) -> tryTraverse sts idx)
    |> fst
    
let solve2 (text:string) =
    let st00a = text |> parse2lines |> Map.ofList
    let st00 = st00a |> Map.add (st00a.Keys |> Seq.max |> (+)1) (11,0)
    let numFall = st00.Keys.Count
    
    Seq.initInfinite id
    |> Seq.scan (fun st _ -> step st) st00
    |> Seq.windowed (numFall+1)
    |> Seq.indexed
    |> Seq.find (fun (idx,sts) -> tryTraverse sts idx)
    |> fst

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()