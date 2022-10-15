module Aoc16.FinishedDays.Test.T00_Practice_Z3_MinSquared
open Aoc16.Practice_Z3_MinSquared

open Aoc16.Input
open NUnit.Framework
open System

[<SetUp>]
let Setup () =
    ()
    
[<Test>]
[<TestCase>]
let Test1() = 
    let sln1 = solve
    Assert.That(sln1, Is.EqualTo "13/10")