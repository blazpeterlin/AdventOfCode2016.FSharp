module Aoc16.D24

open Aoc16
open Input
open TextCopy
open System.Collections.Generic
open Aoc16.Operators
open Microsoft.FSharp.Core.Operators.Checked

let input =  "input.txt" |> f2text

type BLOCK = WALL | SPACE | GOAL of int | OPEN

let parseLine (ln:string) =
    ln.ToCharArray()
    |> ofArray
    |> map (function | '#' -> WALL | '.' -> SPACE | x -> GOAL(x.ToString() |> int))
    |> List.indexed
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine
    |> List.indexed
    |> map (fun (y,lst) -> lst |> map (fun (x,v) -> (x,y),v))
    |> List.concat

let expand (m00:Map<int*int, BLOCK>) =
    let opn = m00 |> Map.toList |> filter (fun (k,v) -> v=OPEN) |> map fst
    let candidates = opn |> map (fun pos -> [pos+..(0,1);pos+..(1,0);pos+..(0,-1);pos+..(-1,0)]) |> List.concat |> HashSet
    m00
    |> Map.toList
    |> map (fun (k,v) ->
            let v2 = match v with | OPEN -> WALL | WALL -> WALL | SPACE -> if candidates.Contains(k) then OPEN else SPACE 
            k,v2
        )
    |> filter (fun (k,v) -> v <> WALL)
    |> Map.ofList

let getAllDists (m00:Map<int*int, BLOCK>) (a:int*int) (bs:(int*int) list) =
    let m0 = m00.Add(a,OPEN)
    let hsb = bs |> HashSet
    m0
    |>Seq.unfold (fun m -> 
        let r = expand m
        Some(r,r))
    |>Seq.map (fun m -> bs |> filter (fun b -> m.ContainsKey(b) && m[b]=OPEN))
    |>Seq.indexed
    |>Seq.map (fun (idx,xs) -> xs |> map (fun x -> (a,x),idx+1))
    |>Seq.concat
    |>Seq.take (bs.Length)
    |>List.ofSeq

let solve1 (text:string) = 
    let inp = text |> parse2lines |> Map
    let niceMap =  inp |> Map.toList |> map (fun (pos,v) -> (pos,match v with | WALL -> WALL | _ -> SPACE)) |> filter (fun (k,v) -> v <> WALL) |> Map.ofList
    let allNums = inp |> Map.toList |> filter (fun (pos,v) -> match v with | GOAL(_) -> true | _ -> false) |> List.sortBy snd
    let starter = allNums[0] |> fst
    let keyPoints = allNums |> map fst
    let permutations = keyPoints |> List.skip 1 |> Math.generateCombinationsNonRepeating

    let allDists = 
        keyPoints
        |> List.map (fun (x) -> 
               let allDists = getAllDists niceMap x (keyPoints |> List.except [x])
               allDists
         )
        |> List.concat
        |> Map.ofList

    let minTotal = 
        seq {
            for perm in permutations do
                let sumDist = 
                    seq {
                        for a,b in List.pairwise(starter::perm) do
                           allDists[a,b]
                    } |> Seq.sum
                sumDist, perm
        }
        |> Seq.minBy fst
        |> fst
    minTotal
    
let solve2 (text:string) =
    let inp = text |> parse2lines |> Map
    let niceMap =  inp |> Map.toList |> map (fun (pos,v) -> (pos,match v with | WALL -> WALL | _ -> SPACE)) |> filter (fun (k,v) -> v <> WALL) |> Map.ofList
    let allNums = inp |> Map.toList |> filter (fun (pos,v) -> match v with | GOAL(_) -> true | _ -> false) |> List.sortBy snd
    let starter = allNums[0] |> fst
    let keyPoints = allNums |> map fst
    let permutations = keyPoints |> List.skip 1 |> Math.generateCombinationsNonRepeating

    let allDists = 
        keyPoints
        |> List.map (fun (x) -> 
                let allDists = getAllDists niceMap x (keyPoints |> List.except [x])
                allDists
            )
        |> List.concat
        |> Map.ofList

    let minTotal = 
        seq {
            for perm in permutations do
                let sumDist = 
                    seq {
                        for a,b in List.pairwise(starter::perm@[starter]) do
                            allDists[a,b]
                    } |> Seq.sum
                sumDist, perm
        }
        |> Seq.minBy fst
        |> fst
    minTotal

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()