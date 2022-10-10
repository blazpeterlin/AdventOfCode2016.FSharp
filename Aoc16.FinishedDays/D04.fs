module Aoc16.D04

open Aoc16
open Input
open TextCopy
open System.Collections.Generic
open Aoc16.Operators

let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    ln
    |> text2tokens "-[]"
    |> fun xs -> (xs |> List.rev |> List.skip 2 |> List.rev |> List.reduce (fun (x:string) (y:string) -> x+"-"+y) ),(xs|>List.rev|>List.item 1|>int),(xs|>List.rev|>List.head)
    // |> text2tokensStr ["abc";"def"]

let isReal (name:string,someNum:int,csum:string) = 
    let str = 
        name.ToCharArray()
        |> ofArray
        |> List.filter (fun ch -> ch <> '-')
        |> List.groupBy id
        |> List.sortBy (fun (k,v) -> -v.Length, k)
        |> List.map fst
        |> map (fun x -> x.ToString())
        |> List.take 5
        |> List.reduce (+)
    str = csum
   
let shiftOnce (name:string) =
    name.ToCharArray()
    |> ofArray
    |> map (fun ch -> 
        if ch = 'Z' then 'A' else
        if ch = '-' then ' ' else
        if ch = ' ' then ' ' else
        (char)((int)ch+1)
    )
    |> Array.ofList
    |> fun chs -> System.String(chs)
    |> string
 
let decrypt (name:string,cipher:int,csum:string) = 
    [ 1 .. cipher]
    |> List.fold (
        fun x _ -> 
            let r = shiftOnce x
            r
    ) name

    

let parse2lines (text:string) = 
    text.ToUpper()
    |> text2lines 
    |> List.map parseLine

let solve1 (text:string) = 
    let inp = text |> parse2lines
    inp |> filter isReal |> map (fun (_,num,_) -> num) |> List.sum
    
    
let solve2 (text:string) =
    let inp = text |> parse2lines
    let decrypted = inp |> filter isReal |> map (fun (name,cipher,csum) -> decrypt (name,cipher,csum), cipher)
    let northPole = decrypted |> List.find (fun (d,_) -> d.Contains("NORTHPOLE") )
    northPole |> snd
    

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())
// 666 to low
let finished = true
()