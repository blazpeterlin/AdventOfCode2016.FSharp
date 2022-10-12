module Aoc16.D10

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
    |> text2tokensStr [" "]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine

let step (givers: Map<string,string*string>) (st:Map<string,int option*int option>) =
    let tryGoodBotKey = st |> Map.tryFindKey (fun k v -> match v with | _, Some x -> true | _,None -> false)
    if tryGoodBotKey = None then None else
    let goodBotKey = tryGoodBotKey.Value
    let tgt1,tgt2 = givers[goodBotKey]

    let low,high=st[goodBotKey]

    st
    |> Map.remove goodBotKey
    |> Map.add tgt1 (
        match Map.tryFind tgt1 st with
        | None -> low,None
        | Some (prev1,prev2) -> if prev1.Value < low.Value then prev1,low else low,prev1
    )
    |> Map.add tgt2 (
        match Map.tryFind tgt2 st with
        | None -> high,None
        | Some (prev1,prev2) -> if prev1.Value < high.Value then prev1,high else high,prev1
    )
    |> Some

let setupStates (inps: string list list) =
    let vals = inps |> filter (fun xs -> xs[0]="value") |> map (fun xs -> int xs[1], "bot " + xs[5])
    let givers = inps |> filter (fun xs -> xs[0]="bot") |> map (fun xs -> xs[0] + " " + xs[1], (xs[5] + " " + xs[6], xs[10] + " " + xs[11])) |> Map.ofList

    
    let st0 = vals |> List.groupBy snd |> map (fun (k,vs) -> k, (vs |> map fst |> List.sort |> fun xs -> Some xs[0],if xs.Length=2 then Some xs[1] else None)) |> Map.ofList
    let states = 
        st0
        |> Seq.unfold (fun st ->
            let r = step givers st
            match r with
            | Some x -> Some(x,x)
            | None -> None
        )
    states

let solve1 (text:string) = 
    let inps = text |> parse2lines
    let states = setupStates inps
    let ourBot = 
        states
        |> Seq.find (fun st -> st.Values.Contains(Some 17, Some 61))
        |> Map.findKey (fun key v -> v = (Some 17, Some 61))
    ourBot |> text2tokens " " |> fun xs -> int xs[1]
    
let solve2 (text:string) =
    let inps = text |> parse2lines
    let states = setupStates inps
    let finalSt = states |> Seq.last
    let outputVals = finalSt |> Map.toList |> filter (fun (x,y) -> x.StartsWith "output") |> map (fun (x,(v,_)) -> x,v.Value) |> Map.ofList
    outputVals["output 0"] * outputVals["output 1"] * outputVals["output 2"]

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()