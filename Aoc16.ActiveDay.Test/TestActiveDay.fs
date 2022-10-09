module Aoc16.ActiveDay.Test

open Aoc16.Input
open Aoc16.ActiveDay
open NUnit.Framework
open System

[<SetUp>]
let Setup () =
    ()
    
[<TestCase("input-TEST.txt", 0)>] 
let Test1 (fn : string, res: int) = 
    let input = fn |> f2text
    let sln1 = solve1 input
    Console.WriteLine(sln1)
    Assert.That (sln1, Is.EqualTo res)

[<TestCase("input-TEST.txt", 0)>] 
let Test2 (fn : string, res: int) = 
    let input = fn |> f2text
    let sln2 = solve2 input
    Console.WriteLine(sln2)
    Assert.That (sln2, Is.EqualTo res)
