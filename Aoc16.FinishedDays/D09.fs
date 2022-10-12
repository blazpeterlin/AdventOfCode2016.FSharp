module Aoc16.D09

open Aoc16
open Input
open TextCopy
open System.Collections.Generic
open Aoc16.Operators
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.Operators.Checked


let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    ln
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let matchRegex (r:Regex) (str:string) =
    
    if r.IsMatch str 
    then 
        let grp = (r.Match(str).Groups |> Seq.cast<Group> |> ofSeq) 
        Some (grp[1].Value, grp[2].Value, grp[3].Value)
    else None
    
let rec step (r:Regex) (str:string) =
    match matchRegex r str with
    | None -> str.Length
    | Some (pref,marker,postf) ->
        let take,times = marker |> text2tokens "()x" |> map int |> fun xs -> xs[0],xs[1]
        pref.Length + take*times + step r (postf.Substring(take))

let rec step2 (r:Regex) (str:string) =
    match matchRegex r str with
    | None -> int64 str.Length,int64 0
    | Some (pref,marker,postf) ->
        let take,times = marker |> text2tokens "()x" |> map int |> fun xs -> xs[0],xs[1]
        let part1 = pref.Length
        if postf.Length < take then failwith "huh"
        let part2,part2times = step2 r (postf.Substring(0, take))
        let part3,part3times = step2 r (postf.Substring(take))
        (int64 part1)+(int64 times)*(int64 part2)+(*times**)  (** part2times*) (int64 part3),(int64 part3times)

let solve1 (text:string) = 
    let inp = text |> parse2lines |> head
    let r = Regex("(.*?)(\(\d*x\d*\))(.*)")
    let res = step r inp
    res
    
let solve2 (text:string) =
    let inp = text |> parse2lines |> head
    let r = Regex("(.*?)(\(\d*x\d*\))(.*)")
    let res = step2 r inp |> fst
    res

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()