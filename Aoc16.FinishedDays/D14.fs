module Aoc16.D14

open Aoc16
open Input
open TextCopy
open System.Collections.Generic
open Aoc16.Operators
open Microsoft.FSharp.Core.Operators.Checked
open System.Text
open System.Security.Cryptography

let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    ln
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine
    |> List.head

let md5 (dataStr : string) : string =
    let data = System.Text.Encoding.ASCII.GetBytes(dataStr)
    use md5 = MD5.Create()
    (StringBuilder(), md5.ComputeHash(data))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string
    //|> fun x -> x.ToLower()

let getRepeaters num (str:string) =
    str.ToCharArray()
    |> Seq.windowed num
    |> Seq.filter (fun xs -> xs |> Seq.distinct |> Seq.length |> (=)1)
    |> Seq.map Seq.head
    |> List.ofSeq

let solve1 (text:string) = 
    let salt = text |> parse2lines    

    let sln = 
        Seq.initInfinite id
        |> Seq.map (fun x -> md5 (salt+x.ToString()))
        |> Seq.map (fun str ->
            let three = getRepeaters 3 str |>List.tryHead
            let fives = getRepeaters 5 str
            three,fives
        )
        |> Seq.windowed 1001
        |> Seq.indexed
        |> Seq.filter (fun (idx,w) ->
            let three = w[0] |> fst
            if three.IsNone then false else
            w |> Seq.skip 1 |> Seq.map snd |> Seq.concat |> Seq.tryFind ((=)three.Value)
            |> function | None ->false | Some _ ->true
        )
        |> Seq.map fst
        |> Seq.take 64
        |> List.ofSeq

    sln
    |> List.last

let repeatMd5 n str =
    [1..n]
    |> Seq.fold (fun x _ -> md5 x) str
    
let solve2 (text:string) =
    let salt = text |> parse2lines  

    let sln = 
        Seq.initInfinite id
        |> Seq.map (fun x -> repeatMd5 2017 (salt+x.ToString()))
        |> Seq.map (fun str ->
            let three = getRepeaters 3 str |>List.tryHead
            let fives = getRepeaters 5 str
            three,fives
        )
        |> Seq.windowed 1001
        |> Seq.indexed
        |> Seq.filter (fun (idx,w) ->
            let three = w[0] |> fst
            if three.IsNone then false else
            w |> Seq.skip 1 |> Seq.map snd |> Seq.concat |> Seq.tryFind ((=)three.Value)
            |> function | None ->false | Some _ ->true
        )
        |> Seq.map fst
        |> Seq.take 64
        |> List.ofSeq

    sln
    |> List.last

//let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()