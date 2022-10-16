module Aoc16.D22

open Aoc16
open Input
open TextCopy
open System.Collections.Generic
open Aoc16.Operators
open Microsoft.FSharp.Core.Operators.Checked

let input =  "input.txt" |> f2text

type Node = { Pos:int*int; Size:int; Used:int; Avail:int; UsedPerctg:int; }

let parseLine (ln:string) =
    ln
    // |> text2tokens "x"
    ///dev/grid/node-x0-y0     85T   66T    19T   77%
    |> text2tokensStr ["/dev/grid/";"node-x";"-y";" ";"T";"%"]
    |> fun xs -> { Pos=(int xs[0], int xs[1]); Size=int xs[2]; Used=int xs[3]; Avail=int xs[4]; UsedPerctg = int xs[5]; }

let parse2lines (text:string) = 
    text
    |> text2lines 
    |> List.skip 2
    |> List.map parseLine

let solve1 (text:string) = 
    let inp = text |> parse2lines
    inp
    |> List.allPairs inp
    |> List.filter (fun (x,y) -> x <> y && x.Used <> 0 && x.Used <= y.Avail)
    |> List.length
    
    
let solve2 (text:string) =
    let inp = text |> parse2lines
    let maxX = inp |> List.map (fun elt -> elt.Pos |> fst) |> List.max

    let (emptyX0,emptyY)= inp |> List.find (fun elt -> elt.Used=0) |> fun elt -> elt.Pos
    
    // hacky solution based on looking at input data - there's a large wall "blocking" starting empty-block from reaching the top row. 
    // all large nodes are part of this horizontal wall and it extends to the far right. Figure out its left-most bit
    let largeWallXs = inp |> List.filter (fun elt -> elt.Used >= 100 && (snd elt.Pos) < emptyY) |> List.map (fun elt -> elt.Pos |> fst)

    let emptyX = if largeWallXs.Length=0 then emptyX0 else (largeWallXs |> List.min) - 1
    let costToEmptyX = emptyX0-emptyX
    let costToSourcePos = emptyY + (maxX-emptyX)
    let costRemainingRoundtrips = 
        seq {
            for endX in maxX-1..-1..1 do
                let tripDown = 1
                let tripLeft = 2
                let tripUp = 1
                let tripRight = 1
                tripDown + tripLeft + tripUp + tripRight
        }
        |> Seq.sum
    costToEmptyX + costToSourcePos + costRemainingRoundtrips

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()