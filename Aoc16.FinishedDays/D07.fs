module Aoc16.D07

open Aoc16
open Input
open TextCopy
open System.Collections.Generic
open Aoc16.Operators

let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    ln
    |> text2tokens "[]"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let isAbba (str:string) =
    str[0]=str[3] && str[1]=str[2] && str[0]<>str[1]

let isAbbaFull (str:string) =
    str.ToCharArray() 
    |> ofArray 
    |> map (fun x -> x.ToString()) 
    |> List.windowed 4 
    |> map (List.reduce (+)) 
    |> fun s -> match List.tryFind isAbba s with | Some x -> true | None -> false

let isTLS (strs:string list) =
    let idxs = strs |> map (isAbbaFull) |> List.indexed |> List.filter snd |> map fst
    let idxsGood = idxs |> filter (fun idx -> idx%2=0)
    let idxsBad = idxs |> filter (fun idx -> idx%2=1)
    idxsGood.Length>0 && idxsBad.Length=0

let isABA (str:string) =
    str[0]=str[2] && str[0]<>str[1]

let getABAfull (strs:string list) =
    strs
    |> map (fun str -> 
        str.ToCharArray() 
        |> ofArray 
        |> map (fun x -> x.ToString()) 
        |> List.windowed 3
        |> map (List.reduce (+)) 
        |> List.choose (fun s -> if isABA s then Some (s[0].ToString() + s[1].ToString()) else None)
    )
    |> List.concat
    |> distinct

let isSSL (strs:string list) =
    let findABA = strs |> List.indexed |> filter (fun (x,_) -> x%2=0) |> map snd |> getABAfull |> List.distinct
    let findBAB = strs |> List.indexed |> filter (fun (x,_) -> x%2=1) |> map snd |> getABAfull |> List.map (fun s -> s[1].ToString()+s[0].ToString()) |> List.distinct
    (List.append findABA findBAB |> List.distinct |> List.length) < (List.length findABA + List.length findBAB)


let solve1 (text:string) = 
    let r = text |> parse2lines |> filter isTLS
    List.length r
    
let solve2 (text:string) =
    let r = text |> parse2lines |> filter isSSL
    List.length r

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()