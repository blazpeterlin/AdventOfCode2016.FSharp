module Aoc16.D17

open Aoc16
open Input
open TextCopy
open System.Collections.Generic
open Aoc16.Operators
open Microsoft.FSharp.Core.Operators.Checked
open System.Text
open System.Security.Cryptography

let input =  "input.txt" |> f2text

type DIR = L | R | U| D

let md5 (dataStr : string) : string =
    let data = System.Text.Encoding.ASCII.GetBytes(dataStr)
    use md5 = MD5.Create()
    (StringBuilder(), md5.ComputeHash(data))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string

let parseLine (ln:string) =
    ln
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine
    |> List.head
    
let possibleDirs (x,y) =
    [
        if x>0 then [L] else [];
        if y>0 then [U] else [];
        if x<3 then [R] else [];
        if y<3 then [D] else [];
    ]
    |> List.concat

let step path pos =
    if pos = (3,3) then [] else
    let allowedDirs = 
        path 
        |> md5 |> fun x -> x.ToCharArray() |> List.ofSeq |> List.take 4
        |> List.indexed
        |> map (fun (idx, ch) -> 
                if (['b';'c';'d';'e';'f'] |> List.contains ch)
                then [match idx with | 0 -> U | 1 -> D | 2 -> L | 3 -> R ] 
                else []
            )
        |> List.concat
    let dirs = 
        possibleDirs pos
        |> List.filter (fun elt -> allowedDirs |> List.contains elt)
    dirs
    |> map (fun dir -> 
        let delta = match dir with | L -> (-1,0) | R -> (1,0) | D -> (0,1) | U -> (0,-1)
        let newPos = delta +.. pos
        path+dir.ToString(), newPos
    )

    
let solve1 (text:string) = 
    let path0 = (text |> parse2lines)
    [path0,(0,0)]
    |> Seq.unfold (fun states -> 
        let r = 
            states
            |> map (fun (str,pos) -> step str pos)
            |> List.concat
        if r.Length=0 then None else Some(r,r)
    )
    |> Seq.choose (fun states -> 
        states
        |> List.tryFind (fun (path,x) -> x=(3,3))
    )
    |> Seq.head
    |> fst
    |> fun str -> str.Substring(path0.Length)
    
    
let solve2 (text:string) =
    let path0 = (text |> parse2lines)
    [path0,(0,0)]
    |> Seq.unfold (fun states -> 
        let r = 
            states
            |> map (fun (str,pos) -> step str pos)
            |> List.concat
        if r.Length=0 then None else Some(r,r)
    )
    |> Seq.choose (fun states -> 
        states
        |> List.tryFind (fun (path,x) -> x=(3,3))
    )
    |> Seq.last
    |> fst
    |> fun str -> str.Substring(path0.Length)
    |> fun str -> str.Length

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res1.ToString())

let finished = true
()