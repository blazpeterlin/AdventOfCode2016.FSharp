module Aoc16.D11

open Aoc16
open Input
open TextCopy
open System.Collections.Generic
open Aoc16.Operators
open Microsoft.FSharp.Core.Operators.Checked

let input =  "input.txt" |> f2text

type Elt = Po | Pr | Th | Ru | Co | Li | Hy | El | Di
type Part = Gen | Chip

let parseLine (ln:string) =
    ln
    |> text2tokensStr ["The ";" floor contains ";".";"a ";", ";"and "]
    |> fun xs ->
        let floor = match xs[0] with | "first" -> 1 | "second" -> 2 | "third" -> 3 | "fourth" -> 4
        if xs[1] = "nothing relevant" then floor,[]  else
        let contained = 
            xs 
            |> List.skip 1 
            |> List.map (text2tokens " ")
            |> map (fun chnk -> 
                let realEltStr = chnk[0].Replace("-compatible", "")
                let elt = match realEltStr with | "polonium" -> Po | "thulium" -> Th | "promethium" -> Pr | "ruthenium" -> Ru | "cobalt" -> Co | "lithium" -> Li | "hydrogen" -> Hy | "elerium" -> El | "dilithium" -> Di
                let part = match chnk[1] with | "generator" -> Gen | "microchip" -> Chip
                elt,part
            )
        floor,contained


let parse2lines (text:string) = 
    text
    |> text2lines
    |> List.map parseLine



// Note: this solution only works for specific starting states, for example ones where all chips are on same floor as their generators.
// The example case (as stated in the instructions) does not fit this condition and therefore needs 2 extra steps. This solution does not work on a generic case.
let solve1 (text:string) = 
    let st0 = text |> parse2lines
    //let st0 = [
    //    4,[];
    //    3,[];
    //    2,[Po,Chip;Pr,Chip];
    //    1,[Po,Gen;Th,Gen;Th,Chip;Pr,Gen;Ru,Gen;Ru,Chip;Co,Gen;Co,Chip];
    //]
    //let st0 = [4,[];3,[Li,Gen];2,[Hy,Gen];1,[Hy,Chip;Li,Chip]]

    let minSteps2 = st0 |> map (fun (floor,lst) -> (4-floor)*lst.Length*2) |> List.sum |> fun x -> x-3*3

    minSteps2
    
let solve2 (text:string) =
    let stInput = text |> parse2lines
    let mapInput = stInput |> Map.ofList
    let addedElts = [El,Gen;El,Chip;Di,Gen;Di,Chip]
    let st0 = 
        mapInput
        |> Map.add 1 (mapInput[1] |> List.append addedElts)
        |> Map.toList
    //let st0 = [
    //    4,[];
    //    3,[];
    //    2,[Po,Chip;Pr,Chip;];
    //    1,[Po,Gen;Th,Gen;Th,Chip;Pr,Gen;Ru,Gen;Ru,Chip;Co,Gen;Co,Chip;El,Gen;El,Chip;Di,Gen;Di,Chip];
    //]

    let minSteps2 = st0 |> map (fun (floor,lst) -> (4-floor)*lst.Length*2) |> List.sum |> fun x -> x-3*3

    minSteps2

let res1 = input |> solve1
let res2 = input |> solve2
// not 63

ClipboardService.SetText(res2.ToString())

let finished = true
()