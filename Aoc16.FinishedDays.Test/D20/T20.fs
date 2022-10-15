module Aoc16.FinishedDays.Test.T20
open Aoc16.D20

open Aoc16.Input
open NUnit.Framework
open System

[<SetUp>]
let Setup () =
    ()

let basePath = System.Reflection.MethodInfo.GetCurrentMethod().DeclaringType.Name.Replace("T","D")
let prependFolder fname = basePath + "\\" + fname
     
[<TestCase("input-TEST.txt", 31053880L)>] 
let Test1 (fn : string, res: int64) = 
    let input = fn |> prependFolder |> f2text
    let sln1 = solve1 input
    Assert.That(sln1, Is.EqualTo res)
    
[<TestCase("input-TEST.txt", 117L)>] 
let Test2 (fn : string, res: int64) = 
    let input = fn |> prependFolder |> f2text
    let sln2 = solve2 input
    Assert.That(sln2, Is.EqualTo res)
