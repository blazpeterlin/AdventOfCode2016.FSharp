module Aoc16.D05

open Aoc16
open Input
open TextCopy
open System.Collections.Generic
open System.Security.Cryptography
open Aoc16.Operators
open System.Text

let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    ln
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine


let md5 (dataStr : string) : string =
    let data = System.Text.Encoding.ASCII.GetBytes(dataStr)
    use md5 = MD5.Create()
    (StringBuilder(), md5.ComputeHash(data))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string

let solve1 (text:string) = 
    let inp = text |> parse2lines |> head
    Seq.initInfinite id
    |> Seq.map (fun i -> inp + i.ToString())
    |> Seq.map md5
    |> Seq.filter (fun r -> r.StartsWith "00000")
    |> Seq.map (fun str -> str[5].ToString())
    |> Seq.take 8
    |> Seq.reduce (+)
    
let solve2 (text:string) = 
    let inp = text |> parse2lines |> head
    Seq.initInfinite id
    |> Seq.map (fun i -> inp + i.ToString())
    |> Seq.map md5
    |> Seq.filter (fun r -> r.StartsWith "00000")
    |> Seq.filter (fun str -> match System.Int32.TryParse (str[5].ToString()) with | true,i -> i >= 0 && i <= 7 | _ -> false)
    |> Seq.map (fun str -> (str[5].ToString() |> int, str[6].ToString()))
    |> Seq.distinctBy fst
    |> Seq.take 8
    |> Seq.sortBy fst
    |> Seq.map snd
    |> Seq.reduce (+)

//let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()