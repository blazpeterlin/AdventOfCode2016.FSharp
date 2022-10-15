module Aoc16.D12

open Aoc16
open Input
open TextCopy
open System.Collections.Generic
open Aoc16.Operators
open Microsoft.FSharp.Core.Operators.Checked

let input =  "input.txt" |> f2text

// decompiled code, not easily testable

let solve1 (text:string) = 
    let mutable a=1;
    let mutable b=1;
    let mutable d=26;
    //let mutable c=7;
    let mutable c=0;

    while c <> 0 do
        d <- d+1
        c <- c-1
    
    while d <> 0 do
        c <- a
        while b <> 0 do
            a <- a+1
            b <- b-1
        b <- c
        d <- d-1

    c <- 19

    while c<>0 do  
        d <- 11
        while d<> 0 do 
            a <- a+1
            d <- d-1
        c <- c-1

    a
    
let solve2 (text:string) =
    let mutable a=1;
    let mutable b=1;
    let mutable d=26;
    let mutable c=7;

    while c <> 0 do
        d <- d+1
        c <- c-1
    
    while d <> 0 do
        c <- a
        while b <> 0 do
            a <- a+1
            b <- b-1
        b <- c
        d <- d-1

    c <- 19

    while c<>0 do  
        d <- 11
        while d<> 0 do 
            a <- a+1
            d <- d-1
        c <- c-1

    a

let res1 = input |> solve1
let res2 = input |> solve2

ClipboardService.SetText(res2.ToString())

let finished = true
()