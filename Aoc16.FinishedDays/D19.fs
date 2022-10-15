module Aoc16.D19

open Aoc16
open Input
open TextCopy
open System.Collections.Generic
open Aoc16.Operators
open Microsoft.FSharp.Core.Operators.Checked

let input =  "input.txt" |> f2text

let parseLine (ln:string) =
    ln |> int
    // |> text2tokens "x"
    // |> text2tokensStr ["abc";"def"]

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.map parseLine
    |> List.head

let next (nextElf:LinkedListNode<int>, dll:LinkedList<int>) =
    dll.Remove(match nextElf.Next with | null -> dll.First | x -> x)
    (match nextElf.Next with |null -> dll.First | x -> x),dll

let getNext (elt:LinkedListNode<int>) =
    match elt.Next with 
    | null -> elt.List.First
    | _ -> elt.Next

let next2 (nextElf:LinkedListNode<int>,oppositeElf:LinkedListNode<int>,dll:LinkedList<int>) =
    let nextOpposite = 
        if dll.Count%2=1
        then oppositeElf |> getNext |> getNext
        else oppositeElf |> getNext
    dll.Remove(oppositeElf)
    (nextElf |> getNext),nextOpposite,dll

let solve1 (text:string) = 
    let numElves = text |> parse2lines
    //let numElves = 5

    let dll0 = List.init numElves ((+)1) |> LinkedList
    let nextElf0 = dll0.First

    (nextElf0,dll0)
    |> Seq.unfold (fun (nextElf, dll) ->
        if dll.Count=1 then None else
        let r = next (nextElf,dll)
        Some(r,r)
    )
    |> Seq.last
    |> fst
    |>fun x -> x.Value
    
let solve2 (text:string) =
    let numElves = text |> parse2lines
    //let numElves = 5

    let dll0 = List.init numElves ((+)1) |> LinkedList
    let nextElf0 = dll0.First
    let oppositeElf0 = dll0.Find(numElves/2+1)

    (nextElf0,oppositeElf0,dll0)
    |> Seq.unfold (fun (nextElf,oppositeElf, dll) ->
        if dll.Count=1 then None else
        let r = next2 (nextElf,oppositeElf,dll)
        Some(r,r)
    )
    |> Seq.last
    |> fun (nxt,op,dll) -> nxt
    |> fun x -> x.Value

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()